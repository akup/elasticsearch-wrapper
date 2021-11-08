package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._

object QueryType extends Enumeration {
  val match_all, `match`, match_phrase, term, terms, range, multi_match, prefix, phrase_prefix, exists,
  geo_distance, bool
  = Value
}
object MultiMatchType extends Enumeration {
  val best_fields, most_fields, cross_fields, phrase, phrase_prefix, bool_prefix = Value
}
object Operator extends Enumeration {
  val OR, AND = Value
}
case class Range(from: Option[Any], fromEq: Boolean = false,
                 to: Option[Any], toEq: Boolean = false,
                 format: Option[DateFormats.Value] = None)
case class FieldQuery(field: Seq[String], query: String,
                      multiMatchType: Option[MultiMatchType.Value] = None,
                      range: Option[Range] = None,
                      analyzer: Option[String] = None)
case class GeoPoint(lat: Double, lon: Double) {
  def toJObject: JObject = {
    JObject("lat" -> lat, "lon" -> lon)
  }
}

abstract class Query(val queryType: QueryType.Value,
                     val analyzer: Option[String]) {
  def toJsonField: JField
}

class MatchAllQuery() extends Query(QueryType.match_all, None) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject())
  }
}
class ExistsQuery(field: String) extends Query(QueryType.exists, None) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject("field" -> field))
  }
}
class TermQuery(field: String, value: Any) extends Query(QueryType.term, None) {
  override def toJsonField: JField = {
    any2JValue(value) match {
      case arr: JArray =>
        JField(QueryType.terms.toString, JObject(field -> arr))
      case x =>
        JField(queryType.toString, JObject(field -> x))
    }
  }
}
class MatchQuery(override val analyzer: Option[String],
                 field: String,
                 query: String,
                 boost: Option[Double] = None,
                 fuzziness: Option[(Int, Boolean)] = None,
                 prefix_length: Option[Int] = None,
                 operator: Option[Operator.Value] = None) extends Query(QueryType.`match`, analyzer) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(
      field -> JObject(
        JField("query", JString(query)) ::
          boost.map(b => JField("boost", JDouble(b))).toList :::
          analyzer.map(a => JField("analyzer", JString(a))).toList :::
          fuzziness.flatMap(f =>
            if (f._2 || f._1 > 0)
              Some(JField("fuzziness", if (f._2) JString("AUTO") else JInt(f._1)))
            else
              None
          ).toList :::
          prefix_length.map(p => JField("prefix_length", JInt(p))).toList :::
          operator.map(o => JField("operator", JString(o.toString))).toList :::
          Nil
      ))
    )
  }
}
class MatchPhraseQuery(override val analyzer: Option[String],
                 field: String,
                 query: String, boost: Option[Double] = None) extends Query(QueryType.match_phrase, analyzer) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(
      field -> JObject(
        ("query" -> query) ::
          boost.map(b => "boost" -> b).toList :::
          analyzer.map(a => "analyzer" -> a).toList :::
          Nil
      )
    ))
  }
}
class PrefixQuery(field: String,
                  query: String) extends Query(QueryType.prefix, None) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(
      field -> query
    ))
  }
}
class RangeQuery(field: String, range: Range) extends Query(QueryType.range, None) {
  override def toJsonField: JField = {
    if (range.from.isEmpty && range.to.isEmpty)
      throw new Exception("range exception")

    JField(queryType.toString, JObject(
      field -> JObject(
        range.from.map(from => (if (range.fromEq) "gte" else "gt") -> from),
          range.to.map(to => (if (range.toEq) "lte" else "lt") -> to),
          range.format.map(f => "format" -> f.toString)
      )
    ))
  }
}
class MultiMatchQuery(override val analyzer: Option[String] = None,
                     query: String, fields: Seq[String],
                     multiMatchType: Option[MultiMatchType.Value] = None, //best_fields
                     fuzziness: Option[(Int, Boolean)] = None,
                     prefix_length: Option[Int] = None,
                     operator: Option[Operator.Value] = None)
  extends Query(QueryType.multi_match, analyzer) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(
      "query" -> query,
        "fields" -> JArray(fields),
        multiMatchType.map(t => "type" -> t.toString),
        analyzer.map(a => "analyzer" -> a),
        fuzziness.map(f => "fuzziness" -> (if (f._2) "AUTO" else f._1)),
        prefix_length.map(p => "prefix_length" -> JInt(p)),
        operator.map(o => "operator" -> o.toString)
    ))
  }
}
class GeoDistanceQuery(field: String, distance: String, point: GeoPoint) extends Query(QueryType.geo_distance, None) {
  override def toJsonField: JField = {

    JField(queryType.toString, JObject(
      "distance" -> JString(distance),
      field -> point.toJObject
    ))
  }
}
class BoolQuery(must: Seq[Query] = Nil,
                filter: Seq[Query] = Nil,
                should: Seq[Query] = Nil,
                must_not: Seq[Query] = Nil) extends Query(QueryType.bool, None) {
  override def toJsonField: JField = {

    JField(queryType.toString, JObject(
      if (must.nonEmpty)
        Some("must" -> JArray(must.map(m => JObject(m.toJsonField))))
      else None,
      if (filter.nonEmpty)
        Some("filter" -> JArray(filter.map(m => JObject(m.toJsonField))))
      else None,
      if (should.nonEmpty)
        Some("should" -> JArray(should.map(m => JObject(m.toJsonField))))
      else None,
      if (must_not.nonEmpty)
        Some("must_not" -> JArray(must_not.map(m => JObject(m.toJsonField))))
      else None
    ))
  }
}


case class DocumentHit(_id: String, _score: Option[Double], _source: JObject, sort: Iterable[JValue])

case class SuggestCompletionQuery(suggesterId: String, field: String, query: String)