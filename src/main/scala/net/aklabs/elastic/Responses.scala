package net.aklabs.elastic

import net.aklabs.helpers.Helpers
import net.aklabs.helpers.JsonHelpers._


object ErrorTypes extends Enumeration {
  val index_not_found_exception, resource_already_exists_exception,
  invalid_index_name_exception, illegal_argument_exception,
  mapper_parsing_exception, parsing_exception,
  ok = Value
}


sealed trait ElasticResponse

abstract class AbstractResponse extends ElasticResponse {
  val json: JValue
  val state: Int
}
abstract class AbstractSuccessResponse extends AbstractResponse {
  val state: Int = 200
}
case class BaseResponse(override val json: JValue, override val state: Int)
  extends AbstractResponse
case class SuccessResponse(override val json: JValue) extends AbstractSuccessResponse
case class Error(_type: ErrorTypes.Value,
                 override val json: JValue,
                 override val state: Int) extends AbstractResponse

case class MappingsResponse(indexMappings: Map[String, (Seq[MappingsInfo], Option[Boolean])],
                            override val json: JValue) extends AbstractSuccessResponse
object MappingsResponse {
  def jFieldToMappingsInfo(mapping: JField): MappingsInfo = {
    val nestedV = mapping.value \ "properties"
    val mappingType = mapping.value \ "type" match {
      case JString(s) => MappingTypes.withName(s)
      case _ if nestedV.isInstanceOf[JObject] => MappingTypes.`object`
    }
    val nested = nestedV match {
      case nestedObj: JObject => nestedObj.children.map(jFieldToMappingsInfo(_)).toSeq
      case _ => Nil
    }
    val analyzer = mapping.value \ "analyzer" match {case JString(s) => Some(s); case _ => None}
    val search_analyzer = mapping.value \ "search_analyzer" match {case JString(s) => Some(s); case _ => None}
    val search_quote_analyzer = mapping.value \ "search_quote_analyzer" match {case JString(s) => Some(s); case _ => None}
    val max_shingle_size = mapping.value \ "max_shingle_size" match {case JInt(s) => Some(s.toInt); case _ => None}
    val boost = mapping.value \ "boost" match {case JInt(b) => Some(b.toInt); case _ => None}
    val coerce = mapping.value \ "coerce" match {case JBool(c) => Some(c); case _ => None}
    val copy_to = mapping.value \ "copy_to" match {
      case JArray(arr) => arr.flatMap(_ match {case JString(s) => Some(s); case _ => None})
      case JString(c) => Seq(c)
      case _ => Nil
    }
    val doc_values = mapping.value \ "doc_values" match {case JBool(d) => Some(d); case _ => None}
    val dynamic = mapping.value \ "dynamic" match {case JBool(d) => Some(d); case _ => None}
    val eager_global_ordinals = mapping.value \ "eager_global_ordinals" match {case JBool(e) => Some(e); case _ => None}
    val enabled = mapping.value \ "enabled" match {case JBool(e) => Some(e); case _ => None}
    val fielddata = mapping.value \ "fielddata" match {case JBool(fd) => Some(fd); case _ => None}
    //fields: Seq[MappingsInfo] = Nil, //multi field feature
    //format: Option[(Option[DateFormats.Value], String)] = None, //для дат
    val ignore_above = mapping.value \ "ignore_above" match {case JInt(i) => Some(i.toInt); case _ => None}
    val ignore_malformed = mapping.value \ "ignore_malformed" match {case JBool(i) => Some(i); case _ => None}
    val index = mapping.value \ "index" match {case JBool(i) => Some(i); case _ => None}
    val index_options = mapping.value \ "index_options" match {case JString(s) => Some(IndexOptions.withName(s)); case _ => None}
    val index_phrases = mapping.value \ "index_phrases" match {case JBool(i) => Some(i); case _ => None}
    //index_prefixes: Option[(Int, Int)] = None, //2-5 - по умолчанию
    val meta: Map[String, String] = mapping.value \ "meta" match {
      case o: JObject => o.children.flatMap(f => f.value match {
        case JString(s) => Some(f.name -> s)
        case _ => None
      }).toMap
      case _ => Map.empty
    }
    val normalizer = mapping.value \ "normalizer" match {case JString(s) => Some(s); case _ => None}
    val norms = mapping.value \ "norms" match {case JBool(b) => Some(b); case _ => None}
    val null_value = mapping.value \ "null_value" match {
      case JBool(b) => Some(b)
      case JInt(i) => Some(i.toInt)
      case JDouble(i) => Some(i.toDouble)
      case JString(s) => Some(s)
      case _ => None
    }
    val position_increment_gap = mapping.value \ "position_increment_gap" match {case JInt(p) => Some(p.toInt); case _ => None}
    //similarity: Option[(Option[Similarities.Value], String)] = None, //BM25
    val store = mapping.value \ "store" match {case JBool(b) => Some(b); case _ => None}
    val term_vector = mapping.value \ "term_vector" match {case JString(s) => Some(TermVectors.withName(s)); case _ => None}
    val contexts = mapping.value \ "contexts" match {
      case JArray(c) => c.map(c => {
        SuggesterContext(c \ "name" match {case JString(s) => s},
          c \ "type" match {case JString(t) => SuggesterContextType.withName(t.toLowerCase())},
          c \ "precition" match {case JDouble(d) => Some(d); case JInt(i) => Some(i.toDouble); case _ => None}
        )
      }).toSeq
      case _ => Nil
    }

    /*
    term_vector: Option[TermVectors.Value] = None //no
     */

    MappingsInfo(mapping.name, mappingType,
      nested = nested.toList,
      analyzer = analyzer,
      search_analyzer = search_analyzer,
      search_quote_analyzer = search_quote_analyzer,
      max_shingle_size = max_shingle_size,
      boost = boost, coerce = coerce, copy_to = copy_to.toList,
      doc_values = doc_values, dynamic = dynamic,
      eager_global_ordinals = eager_global_ordinals, enabled = enabled,
      fielddata = fielddata,
      ignore_above = ignore_above, ignore_malformed = ignore_malformed,
      index = index, index_options = index_options, index_phrases = index_phrases,
      meta = meta,
      normalizer = normalizer, norms = norms,
      null_value = null_value, position_increment_gap = position_increment_gap,
      store = store, term_vector = term_vector, contexts = contexts.toList)
  }
  def fromJson(json: JValue): MappingsResponse = {
    val mappings = json.children.flatMap(jf => {
      jf.value \ "mappings" match {
        case mappingsObj: JObject =>
          val dynamic = mappingsObj \ "dynamic" match {case JBool(b) => Some(b); case _ => None}
          mappingsObj \ "properties" match {
            case mo: JObject =>
              val mappings = mo.children.toList.map(jFieldToMappingsInfo(_))
              Some(jf.name -> (mappings, dynamic))
            case _ => None
          }
        case _ => None
      }
    }).toMap
    MappingsResponse(mappings, json)
  }
}
case class SettingsResponse(analysis: Map[String, AnalysisSettings] = Map.empty,
                            override val json: JValue) extends AbstractSuccessResponse
object SettingsResponse {
  def fromJson(json: JValue): SettingsResponse = {
    val settings = json.children.flatMap(jf => {
      jf.value \ "settings" \ "index" \ "analysis" match {
        case aObj: JObject =>
          val charFilters = Nil//aObj \ "char_filter"
          val tokenizers = Nil//aObj \ "tokenizer"
          val filters = aObj \ "filter" match {
            case fObj: JObject => fObj.children.map(f => {
              val tokenFilterType = f.value \ "type" match {case JString(s) => TokenFilters.withName(s)}
              tokenFilterType match {
                case TokenFilters.asciifolding => new ASCIIFoldingTokenFilter(f.name,
                  preserve_original = f.value \ "preserve_original" match {case JBool(b) => b; case _ => false})
                case TokenFilters.cjk_bigram => new CJKBigramTokenFilter(f.name,
                  ignored_scripts = f.value \ "ignored_scripts" match {
                    case JArray(arr) => arr.map(s => CJKScripts.withName(s.asInstanceOf[JString].textValue())).toSeq
                    case _ => Nil
                  },
                  output_unigrams = f.value \ "output_unigrams" match {case JBool(b) => Some(b); case _ => None}
                )
              }
            })
            case _ => Nil
          }
          val normalizers = Nil//aObj \ "normalizer"
          val analyzers = aObj \ "analyzer" match {
            case fObj: JObject => fObj.children.map(f => {
              val analyzerType = f.value \ "type" match {case JString(s) => Analizers.withName(s)}
              analyzerType match {
                case Analizers.custom => new CustomAnalizer(f.name,
                  tokenizer = f.value \ "tokenizer" match {
                    case JString(t) =>
                      Helpers.tryo{Tokenizers.withName(t)}.toOption -> t
                  },
                  char_filter = f.value \ "char_filter" match {
                    case JArray(chfs) => chfs.map(chf => {
                      val chfName = chf.asInstanceOf[JString].textValue()
                      Helpers.tryo{CharacterFilters.withName(chfName)}.toOption -> chfName
                    }).toSeq
                    case _ => Nil
                  },
                  filter = f.value \ "filter" match {
                    case JArray(fs) => fs.map(f => {
                      val fName = f.asInstanceOf[JString].textValue()
                      Helpers.tryo{TokenFilters.withName(fName)}.toOption -> fName
                    }).toSeq
                    case _ => Nil
                  },
                  position_increment_gap = f.value \ "position_increment_gap" match {
                    case JInt(i) => Some(i.toInt)
                    case _ => None
                  }
                )
                case Analizers.standard =>
                  val (stopwords, stopwords_list) = f.value \ "stopwords" match {
                    case JString(s) => Some(StopWordsByLanguage.withName(s)) -> Nil
                    case JArray(s) => None -> s.map(_.asInstanceOf[JString].textValue()).toSeq
                    case _ => None -> Nil
                  }
                  new StandardAnalizer(f.name,
                    stopwords = stopwords,
                    stopwords_list = stopwords_list,
                    stopwords_path = f.value \ "stopwords_path" match {case JString(s) => Some(s); case _ => None},
                    max_token_length = f.value \ "max_token_length" match {case JInt(i) => Some(i.toInt); case _ => None}
                  )
                case Analizers.stop =>
                  val (stopwords, stopwords_list) = f.value \ "stopwords" match {
                    case JString(s) => Some(StopWordsByLanguage.withName(s)) -> Nil
                    case JArray(s) => None -> s.map(_.asInstanceOf[JString].textValue()).toSeq
                    case _ => None -> Nil
                  }
                  new StopAnalizer(f.name,
                    stopwords = stopwords,
                    stopwords_list = stopwords_list,
                    stopwords_path = f.value \ "stopwords_path" match {case JString(s) => Some(s); case _ => None}
                  )
                case Analizers.pattern =>
                  val (stopwords, stopwords_list) = f.value \ "stopwords" match {
                    case JString(s) => Some(StopWordsByLanguage.withName(s)) -> Nil
                    case JArray(s) => None -> s.map(_.asInstanceOf[JString].textValue()).toSeq
                    case _ => None -> Nil
                  }
                  new PatternAnalizer(f.name,
                    pattern = f.value \ "pattern" match {case JString(s) => Some(s); case _ => None},
                    flags = f.value \ "flags" match {
                      case JString(s) => s.split("\\|").map(JavaRegexFlags.withName(_)).toSeq
                      case _ => Nil
                    },
                    lowercase = f.value \ "pattern" match {case JBool(l) => Some(l); case _ => None},
                    stopwords = stopwords,
                    stopwords_list = stopwords_list,
                    stopwords_path = f.value \ "stopwords_path" match {case JString(s) => Some(s); case _ => None}
                  )
                case Analizers.arabic|Analizers.armenian|Analizers.basque|
                     Analizers.bengali|Analizers.brazilian|Analizers.bulgarian|
                     Analizers.catalan|Analizers.cjk|Analizers.czech|
                     Analizers.danish|Analizers.dutch|Analizers.english|Analizers.estonian|
                     Analizers.finnish|Analizers.french|Analizers.galician|Analizers.german|
                     Analizers.greek|Analizers.hindi|Analizers.hungarian|Analizers.indonesian|
                     Analizers.irish|Analizers.italian|Analizers.latvian|Analizers.lithuanian|
                     Analizers.norwegian|Analizers.persian|Analizers.portuguese|Analizers.romanian|
                     Analizers.russian|Analizers.sorani|Analizers.spanish|Analizers.swedish|
                     Analizers.turkish|Analizers.thai =>
                  val (stopwords, stopwords_list) = f.value \ "stopwords" match {
                    case JString(s) => Some(StopWordsByLanguage.withName(s)) -> Nil
                    case JArray(s) => None -> s.map(_.asInstanceOf[JString].textValue()).toSeq
                    case _ => None -> Nil
                  }
                  new LanguageAnalizers(f.name, LanguageAnalizers.withName(analyzerType.toString),
                    stopwords = stopwords,
                    stopwords_list = stopwords_list,
                    stopwords_path = f.value \ "stopwords_path" match {case JString(s) => Some(s); case _ => None},
                    stem_exclusion = f.value \ "stem_exclusion" match {
                      case JArray(arr) => arr.map(_.asInstanceOf[JString].textValue()).toSeq
                      case _ => Nil
                    })
                case Analizers.fingerprint =>
                  val (stopwords, stopwords_list) = f.value \ "stopwords" match {
                    case JString(s) => Some(StopWordsByLanguage.withName(s)) -> Nil
                    case JArray(s) => None -> s.map(_.asInstanceOf[JString].textValue()).toSeq
                    case _ => None -> Nil
                  }
                  new FingerprintAnalizer(f.name,
                    stopwords = stopwords,
                    stopwords_list = stopwords_list,
                    stopwords_path = f.value \ "stopwords_path" match {case JString(s) => Some(s); case _ => None},
                    separator = f.value \ "separator" match {case JString(s) => Some(s); case _ => None},
                    max_output_size = f.value \ "max_output_size" match {case JInt(i) => Some(i.toInt); case _ => None},
                  )
              }
            })
            case _ => Nil
          }
          Some(jf.name -> AnalysisSettings(
            charFilters = charFilters,
            tokenizers = tokenizers,
            filters = filters,
            normalizers = normalizers,
            analyzers = analyzers
          ))
        case _ => None
      }
    }).toMap
    SettingsResponse(settings, json)
  }
}

case class DocumentResponse(document: JValue,
                             override val json: JValue) extends AbstractSuccessResponse
object DocumentResponse {
  def fromJson(json: JValue): DocumentResponse = {
    DocumentResponse(json \ "_source", json)
  }
}
case class SearchResponse(documents: Seq[DocumentHit], total: Long) extends AbstractSuccessResponse {
  override val json = JNull()
}


case class CompletionResponse(completions: Map[String, Seq[DocumentHit]]) extends AbstractSuccessResponse {
  override val json = JNull()
}
object CompletionResponse {
  def fromJson(json: JValue): CompletionResponse = {
    CompletionResponse(json \ "suggest" match {
      case suggObj: JObject =>
        suggObj.children.flatMap(f => {
          val documents = (f.value match {
            case JArray(a) => a.head
            case o: JObject => o
          }) \ "options" match {case JArray(arr) => arr.map(o => {
            DocumentHit(o \ "_id" match {case JString(s) => s},
              o \ "_score" match {case JDouble(score) => Some(score); case _ => None},
              o \ "_source" match {case o: JObject => o}, Nil)
          }).toSeq}
          if (documents.isEmpty)
            None
          else
            Some(f.name -> documents)
        }).toMap
      case _ =>
        throw new Exception("Invalid completion response, no valid 'suggest' field")
    })
  }
}


case class TokenInfo(token: String, start_offset: Int, end_offset: Int, position: Int,
                     tokenType: TokenTypes.Value, positionLength: Option[Int])
case class TokensResponse(tokens: Seq[TokenInfo]) extends AbstractSuccessResponse {
  override val json = JNull()
}
object TokensResponse {
  def fromJson(json: JValue): TokensResponse = {
    json \ "tokens" match {
      case JArray(arr) => TokensResponse(arr.map(t => {
        TokenInfo(
          t \ "token" match {case JString(t) => t},
          t \ "start_offset" match {case JInt(i) => i.toInt},
          t \ "end_offset" match {case JInt(i) => i.toInt},
          t \ "position" match {case JInt(i) => i.toInt},
          t \ "type" match {case JString(s) =>
            Helpers.tryo{TokenTypes.withName(s)}.getOrElse{
              TokenTypes.withName(s.substring(1, s.length - 1))
            }
          },
          t \ "positionLength" match {case JInt(i) => Some(i.toInt); case _ => None}
        )
      }).toSeq)
    }
  }
}