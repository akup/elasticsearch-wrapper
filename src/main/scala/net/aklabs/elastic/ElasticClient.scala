package net.aklabs.elastic

import java.io.File

import akka.util.Timeout
import net.aklabs.helpers.TimeHelpers._
import com.nn.curl.Curl
import jdk.nashorn.internal.runtime.ScriptObject
import net.aklabs.Props
import net.aklabs.helpers.JsonHelpers._
import net.aklabs.helpers.TerminalExecutor
import org.apache.commons.io.FileUtils
import org.pmw.tinylog.Logger

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.language.postfixOps

object ElasticClient {
  private var _roundRobinHosts: Array[String] = Array()
  private var i = -1
  private def getNextHost: String = {
    i += 1
    if (i >= _roundRobinHosts.length)
      i = 0

    _roundRobinHosts(i)
  }

  private var _inited = false
  def inited: Boolean = _inited
  def init(): Unit = {
    val hosts = Props.get("elasticsearch.hosts").map(_.split(",").toSeq).getOrElse(Nil)
    val ports = Props.get("elasticsearch.ports").map(_.split(",").toSeq).getOrElse(Nil).map(_.toInt)
    if (hosts.size != ports.size)
      throw new Exception("elasticsearch.hosts and elasticsearch.ports should be same size")
    init(hosts.zip(ports))
  }
  def init(urls: Seq[(String, Int)]): Unit = if (!_inited) {
    //Logger.debug("Init elasticsearch: " + urls)
    _roundRobinHosts = urls.map(elastic_host => {
      val host = "%s:%d".format(elastic_host._1,elastic_host._2)
      val result = Curl.execute(host, connection_timeout = Some(5), timeout = Some(10))
      //Logger.debug(result.asString)
      //Logger.debug(result.responseCode)
      if (result.responseCode != 200)
        throw new Exception("Cannot connect to host %s and port %d".format(elastic_host._1, elastic_host._2))
      host
    }).toArray

    val filePath = "%s%s/%s".format(Props.get("service.files").getOrElse("."), "/elastic_scripts", "scp.sh")

    FileUtils.copyInputStreamToFile(getClass.getResourceAsStream("/scp.sh"), new File(filePath))
    /*
    val script = Props.getResourceUrl("/scp.sh").map(url => new File(url.getFile)).get
    FileUtils.copyFile(script, new File(filePath))
     */
    Runtime.getRuntime.exec(
      Array("chmod", "+x", filePath)
    ).waitFor()

    _inited = true
  }


  def copyToConfig(copyFrom: String, copyTo: String): Unit = {
    Logger.debug(copyFrom)
    Logger.debug(copyTo)

    val hosts = Props.get("elasticsearch.hosts").map(_.split(',').toSeq).getOrElse(Nil)
    val volumes = Props.get("elasticsearch.config_volumes").map(_.split(',').toSeq
      .map(_.split(';'))).getOrElse(Nil)
    val users = Props.get("elasticsearch.ssh_user").map(_.split(',').toSeq).getOrElse(Nil)
    val passwords = Props.get("elasticsearch.ssh_pwd").map(_.split(',').toSeq).getOrElse(Nil)
    if (hosts.size != volumes.size || hosts.size != users.size || users.size != passwords.size)
      throw new Exception("invalid elasticsearch properties length")

    var i = 0
    while (i < hosts.size) {
      volumes(i).foreach(v =>
        copyToConfig(copyFrom,
          "/var/lib/docker/volumes/elasticsearch_%s/_data/%s".format(v, copyTo), hosts(i), users(i), passwords(i))
      )
      i += 1
    }
  }
  private def copyToConfig(copyFrom: String, copyTo: String,
                           host: String, user: String, password: String): Unit = {
    val scriptPath = "%s%s/%s".format(Props.get("service.files").getOrElse("."), "/elastic_scripts", "scp.sh")

    Logger.debug("copyToConfig Check file")
    TerminalExecutor.execCommand("ls", Seq("-la", scriptPath))
    val (_, response) = TerminalExecutor.execCommand(scriptPath,
      Seq(host, user, password, copyFrom, copyTo),
      Seq(
        "password" -> "Permission denied.*",
        "permission" -> ".*?Permission denied",
        "wrong_folder" -> ".*No such file or directory",
        "ok" -> "expect: read eof",
        "all" -> ".*")
    )
    if (response.contains("password"))
      throw new Exception("Wrong password")
    if (response.contains("permission"))
      throw new Exception("No permissions")
    if (response.contains("wrong_folder"))
      throw new Exception("Invalid folder to copy to")
  }



  private def jsonToReponse(json: JValue, status: Int, transform: Option[JValue => AbstractResponse] = None): AbstractResponse = {
    if (status == 200)
      transform.map(_(json)).getOrElse(SuccessResponse(json))
    else {
      json \ "error" match {
        case errorObj: JObject =>
          val status = json \ "status" match {
            case JInt(i) => i.toInt
            case _ => throw new Exception("Response should have status")
          }
          Error(ErrorTypes.withName(
            (errorObj \ "type").asInstanceOf[JString].textValue()
          ), json, status)
        case _ =>
          BaseResponse(json, status)
      }
    }
  }
  def getMappings(indexName: String): AbstractResponse = getMappings(Some(indexName))
  def getMappings(indexName: Option[String]): AbstractResponse = if (!_inited) throw new Exception("Not inited") else {
    val mappingsUrl = indexName.map(indexName => {
      if (indexName.indexOf("/") > -1)
        throw new Exception("Index name should not contain '/'")
      "%s/%s/_mapping".format(getNextHost, indexName)
    }).getOrElse("%s/_mapping".format(getNextHost))

    val response = Curl.execute(mappingsUrl)
    val json = Jckson.parse(response.asString)
    /*
    Logger.debug(mappingsUrl)
    Logger.debug("getMappings")
    Logger.debug(json)
     */
    jsonToReponse(json, response.responseCode, Some(MappingsResponse.fromJson(_)))
  }
  def getSettings(indexName: String): AbstractResponse = if (!_inited) throw new Exception("Not inited") else {
    if (indexName.indexOf("/") > -1)
      throw new Exception("Index name should not contain '/'")

    val settingsUrl = "%s/%s/_settings".format(getNextHost, indexName)
    val response = Curl.execute(settingsUrl)
    val json = Jckson.parse(response.asString)
    /*
    Logger.debug("getSettings")
    Logger.debug(json)
     */
    jsonToReponse(json, response.responseCode, Some(SettingsResponse.fromJson(_)))
  }

  private def deltaMappings(newMappings: Seq[MappingsInfo], oldMappings: Seq[MappingsInfo]): Seq[MappingsInfo] = {
    if (oldMappings.isEmpty) newMappings
    else {
      /*
      Logger.debug("om: " + oldMappings.map(_.field).toList)
      Logger.debug("nm: " + newMappings.map(_.field).toList)
      Logger.debug("delta: " + oldMappings.filter(m => !newMappings.exists(_.field == m.field)).toList)
      Logger.debug("forall: " + Nil.forall(_ => true))
       */
      if (oldMappings.size > newMappings.size ||
        !oldMappings.forall(m => newMappings.exists(_.field == m.field))) {
        Logger.debug(oldMappings.size + " : " + newMappings.size)
        Logger.debug(oldMappings.filter(m => !newMappings.exists(_.field == m.field)).toList)
        throw new UpdateMappingException("Delta should remove fields")
      }
      newMappings.flatMap(mapping => {
        oldMappings.find(_.field == mapping.field) match {
          case Some(oldMapping) =>
            val delta = MappingsInfo(oldMapping.field, oldMapping.mappingType,
              deltaMappings(mapping.nested, oldMapping.nested),
              mapping.analyzer.orElse(oldMapping.analyzer),
              mapping.search_analyzer.orElse(oldMapping.search_analyzer),
              mapping.search_quote_analyzer.orElse(oldMapping.search_quote_analyzer),
              mapping.max_shingle_size.orElse(oldMapping.max_shingle_size),
              if (oldMapping.boost.nonEmpty) None else mapping.boost,
              mapping.coerce.orElse(oldMapping.coerce),
              mapping.copy_to,
              if (oldMapping.doc_values.nonEmpty) None else mapping.doc_values,
              mapping.dynamic.orElse(oldMapping.dynamic),
              if (oldMapping.eager_global_ordinals.nonEmpty) None else mapping.eager_global_ordinals,
              if (oldMapping.enabled.nonEmpty) None else mapping.enabled,
              if (oldMapping.fielddata.nonEmpty) None else mapping.fielddata,
              mapping.fields.filter(f => !oldMapping.fields.exists(_.field == f.field)),
              if (oldMapping.format.nonEmpty) None else mapping.format,
              mapping.ignore_above.orElse(oldMapping.ignore_above),
              mapping.ignore_malformed.orElse(oldMapping.ignore_malformed),
              if (oldMapping.index.nonEmpty) None else mapping.index,
              if (oldMapping.index_options.nonEmpty) None else mapping.index_options,
              if (oldMapping.index_phrases.nonEmpty) None else mapping.index_phrases,
              if (oldMapping.index_prefixes.nonEmpty) None else mapping.index_prefixes,
              if (mapping.meta.nonEmpty) mapping.meta else Map.empty,
              if (oldMapping.normalizer.nonEmpty) None else mapping.normalizer,
              mapping.norms.flatMap(n => if (n) None else Some(false)).orElse(oldMapping.norms),
              mapping.null_value.orElse(oldMapping.null_value),
              if (oldMapping.position_increment_gap.nonEmpty) None else mapping.position_increment_gap,
              if (oldMapping.similarity.nonEmpty) None else mapping.similarity,
              if (oldMapping.store.nonEmpty)
                if (oldMapping.store != mapping.store) throw new UpdateMappingException("Can't update store field")
                else None
              else mapping.store,
              if (oldMapping.term_vector.nonEmpty)
                if (oldMapping.term_vector != mapping.term_vector) throw new UpdateMappingException("Can't update term_vector field")
                else None
              else mapping.term_vector,
              if (mapping.contexts.isEmpty) oldMapping.contexts else mapping.contexts
            )
            if (delta.changes_?(oldMapping)) Some(delta) else None
          case _ => Some(mapping)
        }
      })
    }
  }

  def createIndex(indexName: String,
                  mappings: Seq[MappingsInfo] = Nil,
                  sortingSettings: Seq[(String, SortingType.Value)] = Nil,
                  analysisSettings: Option[AnalysisSettings] = None,
                  dynamicMapping: Option[Boolean] = Some(false),
                  checkDelta: Boolean = false): AbstractResponse = if (!_inited) throw new Exception("Not inited") else {
    if (indexName.indexOf("/") > -1)
      throw new Exception("Index name should not contain '/'")

    val updateResponse = if (checkDelta) {
      getMappings(indexName) match {
        case Error(_type, _, _) => None
        case response: MappingsResponse =>
          Logger.debug("Already has index %s".format(indexName))
          analysisSettings.foreach(putSettings(indexName, _) match {
            case Error(_, json, _) =>
              throw new Exception("Invalid settings: " + json.toPrettyString)
            case _ =>
          })
          Some(putMappings(indexName, mappings, dynamicMapping,
            analysisSettings = analysisSettings,
            deltaFrom = response.indexMappings.get(indexName).map(_._1).getOrElse(Nil),
            prevDynamic = response.indexMappings.get(indexName).flatMap(_._2)
          ))
      }
    } else None

    updateResponse.getOrElse{
      val indexSettings = JObject(
        analysisSettings.map(as =>
          JField("settings", JObject(
            (if (sortingSettings.nonEmpty)
              JField("index", JObject(
                JField("sort.field", JArray(sortingSettings.map(el => JString(el._1)))) ::
                  JField("sort.order", JArray(sortingSettings.map(el => JString(el._2.toString)))) :: Nil
              )) :: Nil
            else Nil) :::
            as.toJsonField :: Nil
          )) :: Nil
        ).getOrElse(Nil) :::
          JField("mappings",
            JObject(JField("properties", JObject(
              mappings.map(_.toJsonField(analysisSettings.getOrElse(AnalysisSettings())))
            )) :: dynamicMapping.map(d => JField("dynamic", JBool(d)) :: Nil).getOrElse(Nil))
          ) :: Nil)

      Logger.debug("Create index:")
      Logger.debug(indexSettings.toString)
      val putUrl = "%s/%s".format(getNextHost, indexName)
      val response = Curl.execute(putUrl,
        headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
        postData = Some(indexSettings.toString),
        putRequest = true
      )
      val json = Jckson.parse(response.asString)
      Logger.debug("Create index response: " + response.asString)

      jsonToReponse(json, response.responseCode, None)
    }
  }
  def deleteIndex(indexName: String): AbstractResponse = if (!_inited) throw new Exception("Not inited") else {
    if (indexName.indexOf("/") > -1)
      throw new Exception("Index name should not contain '/'")

    val putUrl = "%s/%s".format(getNextHost, indexName)
    val response = Curl.execute(putUrl, deleteRequest = true)
    val json = Jckson.parse(response.asString)
    //Logger.debug("deleteIndex response: " + json)
    jsonToReponse(json, response.responseCode, None)
  }
  def deleteDocument(indexName: String, docId: String): AbstractResponse = if (!_inited) throw new Exception("Not inited") else {
    if (indexName.indexOf("/") > -1)
      throw new Exception("Index name should not contain '/'")

    val putUrl = "%s/%s/_doc/%s".format(getNextHost, indexName, docId)
    val response = Curl.execute(putUrl, deleteRequest = true)
    val json = Jckson.parse(response.asString)
    jsonToReponse(json, response.responseCode, None)
  }
  def putSettings(indexName: String,
                  analysisSettings: AnalysisSettings) = {
    if (indexName.indexOf("/") > -1)
      throw new Exception("Index name should not contain '/'")

    //TODO: check delta settings
    Logger.debug("putSettings")
    val host = getNextHost
    val closeResponse = Curl.execute("%s/%s/_close".format(host, indexName), postData = Some(""))
    if (closeResponse.responseCode != 200) {
      /*
      Logger.debug("not closed")
      Logger.debug(closeResponse.asString)
      Logger.debug(closeResponse.responseCode)
       */
      jsonToReponse(Jckson.parse(closeResponse.asString), closeResponse.responseCode)
    } else {
      val requestData = JObject(
        analysisSettings.toJsonField :: // analysis
          Nil
      )
      try {
        val putUrl = "%s/%s/_settings".format(host, indexName)
        val response = Curl.execute(putUrl,
          headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
          postData = Some(requestData.toString),
          putRequest = true)
        val json = Jckson.parse(response.asString)

        Logger.debug(putUrl)
        Logger.debug(requestData.toString)
        Logger.debug(response.asString)

        jsonToReponse(json, response.responseCode, None)
      } finally {
        val openResponse = Curl.execute("%s/%s/_open".format(host, indexName), postData = Some(""))
        /*
        Logger.debug("openResponse")
        Logger.debug(openResponse.asString)
        Logger.debug(openResponse.responseCode)
         */
      }
    }
  }
  def putMappings(indexName: String,
                  mappings: Seq[MappingsInfo],
                  dynamicMapping: Option[Boolean] = None,
                  deltaFrom: Seq[MappingsInfo] = Nil,
                  prevDynamic: Option[Boolean] = None,
                  analysisSettings: Option[AnalysisSettings] = None): AbstractResponse = if (!_inited) throw new Exception("Not inited") else {
    if (indexName.indexOf("/") > -1)
      throw new Exception("Index name should not contain '/'")

    val a_settings = analysisSettings.getOrElse{
      if (mappings.flatMap(m => {
        m.analyzer.toList ::: m.search_analyzer.toList ::: m.search_quote_analyzer.toList ::: m.normalizer.toList
      }).nonEmpty) {
        getSettings(indexName) match {
          case s: SettingsResponse => s.analysis.getOrElse(indexName, AnalysisSettings())
        }
      } else AnalysisSettings()
    }

    Logger.debug("NEW MAPPINGS: " + indexName + " : " + mappings)
    Logger.debug("prevMappings: " + deltaFrom.toList)
    val updatedMappings = deltaMappings(mappings, deltaFrom)

    Logger.debug("updatedMappings: " + updatedMappings)
    Logger.debug(prevDynamic + " : " + dynamicMapping)
     

    if (prevDynamic.getOrElse(false) == dynamicMapping.getOrElse(false) && updatedMappings.isEmpty) {
      //Logger.debug("Nothing to update")
      MappingsResponse(Map(indexName -> (deltaFrom, prevDynamic)), JNull())
    } else try {
      Logger.debug("Has something to update")
      val requestData = JObject(JField("properties", JObject(
        updatedMappings.map(_.toJsonField(a_settings))
      )) :: dynamicMapping.map(d => JField("dynamic", JBool(d)) :: Nil).getOrElse(Nil))

      val putUrl = "%s/%s/_mapping".format(getNextHost, indexName)
      val response = Curl.execute(putUrl,
        headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
        postData = Some(requestData.toString),
        putRequest = true)
      val json = Jckson.parse(response.asString)

      Logger.debug(putUrl)
      Logger.debug(requestData.toString)
      Logger.debug(json)

      jsonToReponse(json, response.responseCode, Some(MappingsResponse.fromJson(_)))
    } catch {
      case me: MappingException =>
        try {
          mappings.foreach(_.toJsonField(a_settings))
        } catch {
          case _: MappingException =>
            throw me
        }
        throw new UpdateMappingException("Mappings can not be updated: " + me.getMessage)
    }
  }

  def putDocument(indexName: String, doc: JObject): AbstractResponse = putDocument(Some(indexName), doc = doc)
  def putDocument(indexName: Option[String], documentId: Option[String] = None,
                  doc: JObject): AbstractResponse = {
    var iName = indexName
    var dId = documentId

    val requestData = JObject(doc.children.filter(f => {
      if (f.name == "_index") {
        iName = f.value match {case JString(index) => Some(index)}
        false
      } else if (f.name == "_id") {
        dId = f.value match {case JString(id) => Some(id)}
        false
      } else true
    }))

    val putUrl = "%s/%s/_doc/%s".format(getNextHost, iName.get, dId.get)
    Logger.debug(putUrl)
    Logger.debug(requestData.toString)
    val response = Curl.execute(putUrl,
      headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
      postData = Some(requestData.toString),
      putRequest = true)
    val json = Jckson.parse(response.asString)
    Logger.debug("put document response code: " + response.responseCode)
    Logger.debug("put document response: " + json.toPrettyString)

    jsonToReponse(json, response.responseCode, None)
  }
  def putDocuments(indexName: Option[String],
                   docs: Seq[JObject]): AbstractResponse = {
    if (docs.size == 1) {
      docs.head \ "_id" match {case JString(_) => ; case _ => throw new Exception("No document id")}
      putDocument(indexName, None, docs.head)
    } else {
      val markedDocs = docs.flatMap(doc => {
        var iName = indexName
        var dId: Option[String] = None

        val filteredDoc = JObject(doc.children.filter(f => {
          if (f.name == "_index") {
            iName = f.value match {case JString(index) => Some(index)}
            false
          } else if (f.name == "_id") {
            dId = f.value match {case JString(id) => Some(id); case _ => throw new Exception("No document ID")}
            false
          } else true
        }))

        Seq(
          JObject(JField("index", JObject(
            JField("_index", JString(iName.get)) ::
              JField("_id", JString(dId.get)) :: Nil)) :: Nil),
          filteredDoc
        )
      })

      val putUrl = "%s/_bulk".format(getNextHost)
      val response = Curl.execute(putUrl,
        headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
        postData = Some(markedDocs.mkString("\n").concat("\n")),
        putRequest = true)
      val json = Jckson.parse(response.asString)
      jsonToReponse(json, response.responseCode, None)
    }
  }

  def updateDocument(indexName: String, documentId: String,
                     source: Seq[String],
                     lang: String = "painless",
                     params: Map[String, Any] = Map.empty
                    ): AbstractResponse = {
    val postUrl = "%s/%s/_update/%s".format(getNextHost, indexName, documentId)

    val json = JObject(Seq(JField("script", JObject(Seq(
      JField("source", JString(source.mkString("; "))),
      JField("lang", JString(lang)),
      JField("params", JObject(params.toSeq.map(kv => {
        JField(kv._1, any2JValue(kv._2))
      })))
    )))))

    Logger.debug("update document %s".format(postUrl))
    Logger.debug(json.toPrettyString)

    val response = Curl.execute(postUrl,
      headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
      postData = Some(json.toString)
    )

    Logger.debug("updateDocument response")
    Logger.debug(response.asString)

    jsonToReponse(json, response.responseCode, None)
  }

  def getDocument(indexName: String, documentId: String): AbstractResponse = {
    val putUrl = "%s/%s/_doc/%s".format(getNextHost, indexName, documentId)
    val response = Curl.execute(putUrl)
    val json = Jckson.parse(response.asString)
    jsonToReponse(json, response.responseCode, Some(DocumentResponse.fromJson(_)))
  }

  final val MAX_PAGE_SIZE: Int = 2000
  final val MIN_PAGE_SIZE: Int = 1
  final val MAX_FROM: Int = 2000
  def getDocuments(indexName: String, _size: Int = 50,
                   _from: Option[Int] = None,
                   _search_after: Seq[Any] = Nil,
                   sortBy: Seq[(String, SortingType.Value)] = Nil,
                   sortByScript: Option[(ScriptObject, SortingType.Value, String)] = None,
                   scoreScript: Option[ScriptObject] = None,
                   getAll: Boolean = false,
                   matchAll: Boolean = false,
                   must: Seq[Query] = Nil,
                   filter: Seq[Query] = Nil,
                   should: Seq[Query] = Nil,
                   must_not: Seq[Query] = Nil,
                   sourceFilters: Seq[String] = Nil,
                   track_scores: Boolean = false
                  ): AbstractResponse = {
    val size = if (getAll) MAX_PAGE_SIZE else {
      if (_size > MAX_PAGE_SIZE) MAX_PAGE_SIZE else if (_size < MIN_PAGE_SIZE) MIN_PAGE_SIZE else _size
    }
    val (from, search_after) = if (getAll) (None, Nil) else {
      if (_search_after.nonEmpty && _search_after.size != sortBy.size)
        throw new Exception("search after must be same as sort")
      if (_search_after.nonEmpty && _from.nonEmpty)
        throw new Exception("search after or from can be used")
      (_from, _search_after)
    }
    from.foreach(f => if (f > MAX_FROM) throw new Exception("from search can not be higher than %d".format(MAX_FROM)))

    if (!matchAll && must.isEmpty && filter.isEmpty && should.isEmpty && must_not.isEmpty)
      throw new Exception("search is not defined")
    if (getAll && sortBy.isEmpty)
      throw new Exception("To get all sort field should be defined")

    val query_field = JField("query", JObject(
      (if (matchAll) JField("match_all", JObject(Nil)) :: Nil
      else JField("bool", JObject(
        (if (must.nonEmpty)
          JField("must", JArray(must.map(m => JObject(Seq(m.toJsonField))))) :: Nil
        else Nil) :::
          (if (filter.nonEmpty)
            JField("filter", JArray(filter.map(m => JObject(Seq(m.toJsonField))))) :: Nil
          else Nil) :::
          (if (should.nonEmpty)
            JField("should", JArray(should.map(m => JObject(Seq(m.toJsonField))))) :: Nil
          else Nil) :::
          (if (must_not.nonEmpty)
            JField("must_not", JArray(must_not.map(m => JObject(Seq(m.toJsonField))))) :: Nil
          else Nil) :::
          Nil
      )) :: Nil) ::: Nil
    ))

    val searchQuery = JObject(
      JField("size", JInt(size)) ::
      from.map(f => JField("from", JInt(f)) :: Nil).getOrElse(Nil) :::
        (if (search_after.nonEmpty) JField("search_after", JArray(search_after.map(s =>
          any2JValue(s)
        ))) :: Nil else Nil) :::
        sortByScript.map(sortByScript => {
          JField("sort", JObject(Seq(JField("_script", JObject(Seq(
            JField("type", JString(sortByScript._3)),
            sortByScript._1.toJsonField,
            JField("order", JString(sortByScript._2.toString))
          )))))) :: Nil
        }).getOrElse{
          if (sortBy.nonEmpty) JField("sort", JArray(sortBy.map(s =>
            JObject(Seq(JField(s._1, JString(s._2.toString))))
          ))) :: Nil else Nil
        } :::
        (if (sourceFilters.nonEmpty)
          JField("_source", JArray(sourceFilters.map(JString(_)))) :: Nil
        else Nil) :::
        (if (track_scores) JField("track_scores", JBool(true)) :: Nil
        else Nil) :::
        scoreScript.map(scoreScr => {
          JField("query", JObject(Seq(
            JField("script_score", JObject(Seq(
              query_field,
              scoreScr.toJsonField
            )))
          )))
        }).getOrElse(query_field) :: Nil
    )

    Logger.debug(searchQuery)

    val t1 = System.nanoTime()

    val postUrl = "%s/%s/_search".format(getNextHost, indexName)
    val response = Curl.execute(postUrl,
      headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
      postData = Some(searchQuery.toString))
    var json = Jckson.parse(response.asString)


/*
    Logger.debug("search query:")
    Logger.debug(searchQuery.toString)
*/

    /*
    Logger.debug("json response:")
    Logger.debug(json.toString)
*/

    var (total, list) = json \ "hits" match {
      case hitsObj: JObject =>
        parseJsonHits(hitsObj)
      case _ =>
        return jsonToReponse(json, response.responseCode, None)
    }
    if (getAll) {
      var read = 0L
      var readList = Nil
      val order = sortBy.head._2
      read = list.size
      var search_after = if (list.nonEmpty) {
        list.last.sort
        /*
        if (order == SortingType.asc)
          list.last.sort
        else
          list.head.sort
         */
      } else Nil

      while (read == size) {
        val nextQuery = searchQuery + JField("search_after", JArray(search_after.toSeq))
        //Logger.debug("nextQuery: " + nextQuery + " : " + total)
        val response = Curl.execute(postUrl,
          headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
          postData = Some(nextQuery.toString))
        json = Jckson.parse(response.asString)

        //Logger.debug("next response: " + json.toString)

        val (_total, list_add) = json \ "hits" match {
          case hitsObj: JObject =>
            parseJsonHits(hitsObj)
          case _ =>
            return jsonToReponse(json, response.responseCode, None)
        }
        total = _total
        read = list_add.size
        search_after = if (order == SortingType.asc)
          list_add.last.sort
        else
          list_add.head.sort
        list ++= list_add
      }
    }

    //Logger.debug("Search time: " + (System.nanoTime() - t1))

    SearchResponse(list, total)
  }
  def getAllDocuments(indexName: String,
                   sortBy: Seq[(String, SortingType.Value)], _size: Int = 200, search_after: Seq[Any] = Nil,
                   sortByScript: Option[(ScriptObject, SortingType.Value, String)] = None,
                   scoreScript: Option[ScriptObject] = None,
                   matchAll: Boolean = false,
                   must: Seq[Query] = Nil,
                   filter: Seq[Query] = Nil,
                   should: Seq[Query] = Nil,
                   must_not: Seq[Query] = Nil,
                   sourceFilters: Seq[String] = Nil,
                   track_scores: Boolean = false//, lazyLoad: Boolean = false
                  )(implicit executor: ExecutionContext): (Stream[DocumentHit], Long)  = {
    if (sortBy.isEmpty)
      throw new Exception("To get all sort field should be defined")
    val order = sortBy.head._2

    var total = 0L
    def nextStreamPart(search_after: Seq[Any] = Nil, collected: Long = 0): Future[Stream[DocumentHit]] = {
      Logger.debug("nextStreamPart")
      val promise = Promise[Stream[DocumentHit]]()
      Future {
        try {
          Logger.debug("nextStreamPart get docs")
          val docs = getDocuments(indexName, _size = _size,
            _search_after = search_after,
            sortBy = sortBy, sortByScript = sortByScript, scoreScript = scoreScript,
            matchAll = matchAll, must = must, filter = filter, should = should, must_not = must_not,
            sourceFilters = sourceFilters, track_scores = track_scores) match {
            case SearchResponse(_list, _total) if _list.nonEmpty =>
              total = _total
              val list = _list.toList
              //Logger.debug("Sort: " + list.map(_.sort))
              val search_after = list.last.sort

              val totalCollected = collected + list.size
              if ((_total < 10000 && totalCollected < _total) ||
                (_total >= 10000 && list.size == _size)) {
                /*
                if (lazyLoad) {
                  Stream(list: _*) #::: Stream.continually(Await.result({
                    nextStreamPart(search_after.toList, totalCollected)
                  }, Timeout(60 seconds).duration)).flatMap(x => x)
                } else {
                  Stream(list: _*) #::: Await.result({
                    nextStreamPart(search_after.toList, totalCollected)
                  }, Timeout(60 seconds).duration)
                }
                 */
                Stream(list: _*) #::: Await.result({
                  nextStreamPart(search_after.toList, totalCollected)
                }, Timeout(60 seconds).duration)
              } else Stream(list: _*)
            case _ =>
              Stream.empty
          }
          promise.success(docs)
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            promise.failure(e)
        }
      }

      promise.future
    }
    Await.result(nextStreamPart(search_after), Timeout(60 seconds).duration) -> total
  }
  def getTransformedDocuments[T](indexName: String, transform: Seq[DocumentHit] => Seq[T], _size: Int = 50,
                              sortBy: Seq[(String, SortingType.Value)],
                              _search_after: Seq[Any] = Nil,
                              sortByScript: Option[(ScriptObject, SortingType.Value, String)] = None,
                              scoreScript: Option[ScriptObject] = None,
                              matchAll: Boolean = false,
                              must: Seq[Query] = Nil,
                              filter: Seq[Query] = Nil,
                              should: Seq[Query] = Nil,
                              must_not: Seq[Query] = Nil,
                              sourceFilters: Seq[String] = Nil,
                              track_scores: Boolean = false
                             )(implicit executor: ExecutionContext): (Stream[T], Long) = {
    var total = 0L
    def nextStreamPart(search_after: Seq[Any] = Nil, _collected: Int, _collectedTotal: Int): Future[Stream[T]] = {
      val promise = Promise[Stream[T]]()
      Future {
        try {
          val docs = getDocuments(indexName, _size = _size,
            _search_after = search_after,
            sortBy = sortBy, sortByScript = sortByScript, scoreScript = scoreScript,
            matchAll = matchAll, must = must, filter = filter, should = should, must_not = must_not,
            sourceFilters = sourceFilters, track_scores = track_scores) match {
            case SearchResponse(list, _total) if list.nonEmpty =>
              total = _total
              val transformed = transform(list)
              val collected = _collected + transformed.size
              val collectedTotal = _collectedTotal + list.size
              if (collected < _size &&
                ((_total < 10000 && collectedTotal < _total) ||
                  (_total >= 10000 && list.size == _size))) {
                //Logger.debug("Sort: " + list.map(_.sort))
                val search_after = list.last.sort
                //val future = nextStreamPart(search_after.toList, collected, collectedTotal)
                Stream(transformed: _*) #::: Await.result({
                  nextStreamPart(search_after.toList, collected, collectedTotal)
                },
                  Timeout(60 seconds).duration
                )
              } else
                Stream(transformed: _*)
            case _ =>
              Stream.empty
          }
          promise.success(docs)
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            promise.failure(e)
        }
      }

      promise.future
    }
    Await.result(
      nextStreamPart(_search_after, _collected = 0, _collectedTotal = 0),
      Timeout(60 seconds).duration
    ) -> total
  }


  private def parseJsonHits(hitsObj: JObject): (Long, Seq[DocumentHit]) = {
    val total = hitsObj \ "total" \ "value" match {case JInt(total) => total.toLong; case _ => 0L}
    val list = hitsObj \ "hits" match {
      case JArray(hits) =>
        hits.toSeq.map(h => {
          //Logger.debug("parseJsonHits: " + h)
          val _id = h \ "_id" match {case JString(id) => id}
          val _score = h \ "_score" match {case JDouble(score) => Some(score); case _ => None}
          val _source = h \ "_source" match {case o: JObject => o}
          val sort = h \ "sort" match {case JArray(arr) => arr; case _ => Nil}
          //Logger.debug("_score: " + _score)
          DocumentHit(_id, _score, _source, sort)
        })
      case _ => Nil
    }
    (total, list)
  }

  def completion(indexName: String,
                 query: Seq[SuggestCompletionQuery], size: Int = 5,
                 contexts: Seq[(String, Seq[String])]): AbstractResponse = {
    val request = JObject(Seq(JField("suggest", JObject(query.map(suggQ => {
      JField(suggQ.suggesterId, JObject(Seq(
        JField("prefix", JString(suggQ.query)),
        JField("completion", JObject(
          JField("field", JString(suggQ.field)) ::
            JField("size", JInt(size)) ::
            (if (contexts.isEmpty) Nil
            else JField("contexts", contexts.map(c => {
              JField(c._1, JArray(c._2.map(JString(_))))
            })) :: Nil)
        ))
      )))
    })))))

    Logger.debug("completion request: " + indexName)
    Logger.debug(request.toString)

    val postUrl = "%s/%s/_search".format(getNextHost, indexName)
    val response = Curl.execute(postUrl,
      headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
      postData = Some(request.toString))
    val json = Jckson.parse(response.asString)

    Logger.debug("completion response")
    Logger.debug(json.toString)

    jsonToReponse(json, response.responseCode, Some(CompletionResponse.fromJson(_)))
  }

  def analyze(indexName: String, analyzer: String, query: String): AbstractResponse = {
    val postUrl = "%s/%s/_analyze".format(getNextHost, indexName)
    val request = JObject(Seq(
      JField("analyzer", JString(analyzer)),
      JField("text", JString(query))
    ))
    Logger.debug("analyze")
    Logger.debug(postUrl)
    Logger.debug(request.toString)
    val response = Curl.execute(postUrl,
      headers = Map("Content-Type" -> "application/x-ndjson;charset=UTF-8"),
      postData = Some(request.toString))
    val json = Jckson.parse(response.asString)
    jsonToReponse(json, response.responseCode, Some(TokensResponse.fromJson(_)))
  }

  def phoneticFuzzySearch(indexName: String, query: String,
                          searchFields: Seq[String], fields: Seq[String],
                          phoneticAnalyzer: Option[String],
                          phoneticFields: Seq[String], fuzziness: Option[(Int, Boolean)],
                          size: Int = 10, contexts: Seq[(String, Any)] = Nil,
                          phoneticStopWords: Seq[String] = Nil,
                          weightField: Option[String] = None,
                          sortBy: Seq[(String, SortingType.Value)] = Nil, _search_after: Seq[Any] = Nil) = {
    if (_search_after.nonEmpty && sortBy.isEmpty)
      throw new Exception("Не возможно осуществить поиск с параметром search_after, если не задан параметр сортировки")

    if (phoneticFields.nonEmpty && phoneticAnalyzer.isEmpty)
      throw new Exception("Phonetic analyzer should be provided")

    val (phoneticQuery, phonetic_tokens) = if (fuzziness.exists(f => f._1 == 0 && !f._2)) {
      Some(query) -> None
    } else None -> phoneticAnalyzer.map(a => analyze(indexName, a, query) match {
      case TokensResponse(tokens) =>
        val (nonStopTokens, stopTokens) = tokens.partition(t => !phoneticStopWords.contains(t.token))
        val groupedTokens = tokens.groupBy(_.position).toSeq.map(t => {
          t._1 -> t._2.filter(_.token.length > 1)
        }).filter(_._2.nonEmpty).sortWith((a, b) => a._1 < b._1)//.take(2)
        groupedTokens// -> stopTokens
      case x =>
        throw new Exception("Can not tokenize: %s".format(x.toString))
    })

    getDocuments(indexName, size,
      _search_after = _search_after,
      sortBy = sortBy,
      scoreScript = weightField.map(wf => {
        new ScriptObject(source = Some("doc['%s'].value * _score / 1000".format(wf)))
      }),
      filter = contexts.map(c => {
        new TermQuery(c._1, c._2)
      }),
      should = searchFields.map(field =>
        new MatchQuery(None, field, query, boost = Some(0.8))
      ) ++ fields.map(field =>
        new MatchPhraseQuery(None, field, query, boost = Some(1))
      ) ++ phonetic_tokens.map(_.flatMap(_._2.map(_.token)).mkString(" ")).map(tokensQ => {
        if (tokensQ.length > 0) {
          phoneticFields.map(field =>
            new MatchQuery(analyzer = Some("standard"), field, tokensQ,
              fuzziness = fuzziness.orElse{Some(0 -> true)},
              boost = Some(0.3))
          )
        } else
          Nil
      }).getOrElse(Nil) ++ phoneticQuery.map(q => {
        phoneticFields.map(field =>
          new MatchQuery(None, field, q,
            boost = Some(0.3))
        )
      }).getOrElse(Nil) )
  }
  def phoneticFuzzySearchTransformed[T](indexName: String, transform: Seq[DocumentHit] => Seq[T],
                                     sortBy: Seq[(String, SortingType.Value)],
                                     query: String,
                          searchFields: Seq[String], fields: Seq[String],
                          phoneticAnalyzer: Option[String],
                          phoneticFields: Seq[String], fuzziness: Option[(Int, Boolean)],
                          size: Int = 10, contexts: Seq[(String, Any)] = Nil,
                          phoneticStopWords: Seq[String] = Nil,
                          weightField: Option[String] = None)(implicit executor: ExecutionContext): (Stream[T], Long) = {
    var total = 0L
    def nextStreamPart(search_after: Seq[Any] = Nil, _collected: Int, _collectedTotal: Int): Future[Stream[T]] = {
      val promise = Promise[Stream[T]]()
      Future {
        try {
          val docs = phoneticFuzzySearch(indexName,
            _search_after = search_after,
            sortBy = sortBy, query = query,
            searchFields = searchFields, fields = fields, phoneticAnalyzer = phoneticAnalyzer,
            phoneticFields = phoneticFields, fuzziness = fuzziness,
            contexts = contexts, phoneticStopWords = phoneticStopWords,
            weightField = weightField) match {
            case SearchResponse(list, _total) if list.nonEmpty =>
              total = _total
              val transformed = transform(list)
              val collected = _collected + transformed.size
              val collectedTotal = _collectedTotal + list.size
              if (collectedTotal < _total && collected < size) {
                Logger.debug("Sort: " + list.map(_.sort))
                val search_after = list.last.sort
                val future = nextStreamPart(search_after.toList, collected, collectedTotal)
                Stream(transformed: _*) #::: Await.result(future, Timeout(60 seconds).duration)
              } else
                Stream(transformed: _*)
            case _ =>
              Stream.empty
          }
          promise.success(docs)
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            promise.failure(e)
        }
      }

      promise.future
    }
    Await.result(
      nextStreamPart(Nil, _collected = 0, _collectedTotal = 0),
      Timeout(60 seconds).duration
    ) -> total
  }

  def phoneticFuzzySearchAsYouType(indexName: String, phoneticAnalyzer: String, query: String,
                                   fields: Seq[String],
                                   multiMatchFields: Seq[(String, Boolean)],
                                   size: Int = 10,
                                   contexts: Seq[(String, Any)] = Nil,
                                   phoneticStopWords: Seq[String] = Nil,
                                   weightField: Option[String] = None,
                                   sortBy: Seq[(String, SortingType.Value)] = Nil, _search_after: Seq[Any] = Nil
                                  ): AbstractResponse = {
    if (_search_after.nonEmpty && sortBy.isEmpty)
      throw new Exception("Не возможно осуществить поиск с параметром search_after, если не задан параметр сортировки")
    //Logger.debug("phoneticStopWords: " + phoneticStopWords)
    try {
      analyze(indexName, phoneticAnalyzer, query) match {
        case TokensResponse(tokens) =>
          val (nonStopTokens, stopTokens) = tokens.partition(t => !phoneticStopWords.contains(t.token))

        Logger.debug("nonStopTokens")
        Logger.debug(nonStopTokens.toList)
        Logger.debug("stopTokens")
        Logger.debug(stopTokens.toList)

          val groupedTokens = nonStopTokens.groupBy(_.position).toSeq.map(t => {
            t._1 -> t._2.filter(_.token.length > 1)
          }).filter(_._2.nonEmpty).sortWith((a, b) => a._1 < b._1).take(2)
          Logger.debug("groupedTokens")
          Logger.debug(groupedTokens)
          if (groupedTokens.nonEmpty || stopTokens.nonEmpty) {
            val (fSeq, shouldFilter) = if (groupedTokens.size > 1) {
              val shouldFilter = groupedTokens.head._2.map(_.token.length).exists(_ > 3)
              Seq(new MultiMatchQuery(
                analyzer = Some("standard"),
                query = groupedTokens.head._2.map(_.token).mkString(" "),
                fields = multiMatchFields.map(_._1),
                fuzziness = Some(0 -> true)
              )) -> shouldFilter
            } else Nil -> false

            val (seqCont, cont) = contexts.partition(_._2 match {
              case s: Seq[_] => true
              case s: Array[_] => true
              case _ => false
            })
            getDocuments(indexName, size,
              _search_after = _search_after,
              sortBy = sortBy,
              scoreScript = weightField.map(wf => {
                new ScriptObject(source = Some("if (params._source['weight'] == null) {params._source['weight'] = 1;} params._source['weight'] * _score / 10.0".format(wf)))
              }),
              filter = cont.map(c => {
                new TermQuery(c._1, c._2)
              }) ++ (if (groupedTokens.size > 1 && shouldFilter) fSeq else Nil),
              should = (if (groupedTokens.nonEmpty) fields.map(field =>
                new PrefixQuery(field, query.split("""\s""").filter(_.nonEmpty).head.toLowerCase())
              ) else Nil) ++ fields.map(field =>
                new MatchPhraseQuery(None, field, query, boost = Some(1))
              ) ++ (if (stopTokens.nonEmpty) Seq(
                new MultiMatchQuery(
                  analyzer = Some("standard"),
                  query = stopTokens.map(_.token).mkString(" "),
                  fields = multiMatchFields.map(_._1),
                  fuzziness = Some(0 -> false)
                )
              ) else Nil) ++ (if (groupedTokens.size > 1 && !shouldFilter) fSeq else Nil),
              must = (if (groupedTokens.isEmpty) Nil else Seq(new MultiMatchQuery(
                analyzer = Some("standard"),
                query = if (groupedTokens.size > 1)
                  groupedTokens.last._2.map(_.token).mkString(" ")
                else
                  groupedTokens.head._2.map(_.token).mkString(" "),
                fields = multiMatchFields.map(_._1),
                fuzziness = Some(0 -> false)
              ))) ++ seqCont.map(c => new BoolQuery(should = c._2 match {
                case s: Seq[_] => s.map(new TermQuery(c._1, _))
                case s: Array[_] => s.toSeq.map(new TermQuery(c._1, _))
                case _ => Nil
              }))
            )
          } else SearchResponse(Nil, 0)

        case x =>
          throw new Exception("Can not tokenize: %s".format(x.toString))
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }
  }
  def phoneticFuzzySearchAsYouTypeTransformed[T](indexName: String, transform: Seq[DocumentHit] => Seq[T],
                                                 sortBy: Seq[(String, SortingType.Value)],
                                                 phoneticAnalyzer: String, query: String,
                                                 fields: Seq[String],
                                                 multiMatchFields: Seq[(String, Boolean)],
                                                 size: Int = 5,
                                                 contexts: Seq[(String, Any)] = Nil,
                                                 phoneticStopWords: Seq[String] = Nil,
                                                 weightField: Option[String] = None)(implicit executor: ExecutionContext): (Stream[T], Long) = {
    var total = 0L
    def nextStreamPart(search_after: Seq[Any] = Nil, _collected: Int, _collectedTotal: Int): Future[Stream[T]] = {
      val promise = Promise[Stream[T]]()
      Future {
        try {
          val docs = phoneticFuzzySearchAsYouType(indexName, size = size,
            _search_after = search_after,
            sortBy = sortBy, phoneticAnalyzer = phoneticAnalyzer, query = query,
            fields = fields, multiMatchFields = multiMatchFields,
            contexts = contexts, phoneticStopWords = phoneticStopWords,
            weightField = weightField) match {
            case SearchResponse(list, _total) if list.nonEmpty =>
              total = _total
              val transformed = transform(list)
              val collected = _collected + transformed.size
              val collectedTotal = _collectedTotal + list.size
              if (collectedTotal < _total && collected < size) {
                Logger.debug("Sort: " + list.map(_.sort))
                val search_after = list.last.sort
                val future = nextStreamPart(search_after.toList, collected, collectedTotal)
                Stream(transformed: _*) #::: Await.result(future, Timeout(60 seconds).duration)
              } else
                Stream(transformed: _*)
            case _ =>
              Stream.empty
          }
          promise.success(docs)
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            promise.failure(e)
        }
      }

      promise.future
    }
    Await.result(
      nextStreamPart(Nil, _collected = 0, _collectedTotal = 0),
      Timeout(60 seconds).duration
    ) -> total
  }
}