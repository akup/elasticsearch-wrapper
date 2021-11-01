package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers.{JArray, JDouble, JField, JInt, JObject, JString, JValue, any2JValue}

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
    JObject(Seq(JField("lat", JDouble(lat)), JField("lon", JDouble(lon))))
  }
}

abstract class Query(val queryType: QueryType.Value,
                     val analyzer: Option[String]) {
  def toJsonField: JField
}

class MatchAllQuery() extends Query(QueryType.match_all, None) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(Nil))
  }
}
class ExistsQuery(field: String) extends Query(QueryType.exists, None) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(Seq(JField("field", JString(field)))))
  }
}
class TermQuery(field: String, value: Any) extends Query(QueryType.term, None) {
  override def toJsonField: JField = {
    any2JValue(value) match {
      case arr: JArray =>
        JField(QueryType.terms.toString, JObject(Seq(JField(field, arr))))
      case x =>
        JField(queryType.toString, JObject(Seq(JField(field, x))))
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
    JField(queryType.toString, JObject(Seq(
      JField(field, JObject(
        JField("query", JString(query)) ::
          boost.map(b => JField("boost", JDouble(b)) :: Nil).getOrElse(Nil) :::
          analyzer.map(a => JField("analyzer", JString(a)) :: Nil).getOrElse(Nil) :::
          fuzziness.map(f =>
            if (f._2 || f._1 > 0)
              JField("fuzziness", if (f._2) JString("AUTO") else JInt(f._1)) :: Nil
            else
              Nil
          ).getOrElse(Nil) :::
          prefix_length.map(p => JField("prefix_length", JInt(p)) :: Nil).getOrElse(Nil) :::
          operator.map(o => JField("operator", JString(o.toString)) :: Nil).getOrElse(Nil) :::
          Nil
      ))
    )))
  }
}
class MatchPhraseQuery(override val analyzer: Option[String],
                 field: String,
                 query: String, boost: Option[Double] = None) extends Query(QueryType.match_phrase, analyzer) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(Seq(
      JField(field, JObject(
        JField("query", JString(query)) ::
          boost.map(b => JField("boost", JDouble(b)) :: Nil).getOrElse(Nil) :::
          analyzer.map(a => JField("analyzer", JString(a)) :: Nil).getOrElse(Nil) :::
          Nil
      ))
    )))
  }
}
class PrefixQuery(field: String,
                  query: String) extends Query(QueryType.prefix, None) {
  override def toJsonField: JField = {
    JField(queryType.toString, JObject(Seq(
      JField(field, JString(query))
    )))
  }
}
class RangeQuery(field: String, range: Range) extends Query(QueryType.range, None) {
  override def toJsonField: JField = {
    if (range.from.isEmpty && range.to.isEmpty)
      throw new Exception("range exception")

    JField(queryType.toString, JObject(Seq(
      JField(field, JObject(
        range.from.map(from => JField(if (range.fromEq) "gte" else "gt", any2JValue(from)) :: Nil).getOrElse(Nil) :::
          range.to.map(to => JField(if (range.toEq) "lte" else "lt", any2JValue(to)) :: Nil).getOrElse(Nil) :::
          range.format.map(f => JField("format", JString(f.toString)) :: Nil).getOrElse(Nil)
      ))
    )))
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
      JField("query", JString(query)) ::
        JField("fields", JArray(fields.map(JString(_)))) ::
        multiMatchType.map(t => JField("type", JString(t.toString)) :: Nil).getOrElse(Nil) :::
        analyzer.map(a => JField("analyzer", JString(a)) :: Nil).getOrElse(Nil) :::
        fuzziness.map(f => JField("fuzziness", if (f._2) JString("AUTO") else JInt(f._1)) :: Nil).getOrElse(Nil) :::
        prefix_length.map(p => JField("prefix_length", JInt(p)) :: Nil).getOrElse(Nil) :::
        operator.map(o => JField("operator", JString(o.toString)) :: Nil).getOrElse(Nil) :::
        Nil
    ))
  }
}
class GeoDistanceQuery(field: String, distance: String, point: GeoPoint) extends Query(QueryType.geo_distance, None) {
  override def toJsonField: JField = {

    JField(queryType.toString, JObject(Seq(
      JField("distance", JString(distance)),
      JField(field, point.toJObject)
    )))
  }
}
class BoolQuery(must: Seq[Query] = Nil,
                filter: Seq[Query] = Nil,
                should: Seq[Query] = Nil,
                must_not: Seq[Query] = Nil) extends Query(QueryType.bool, None) {
  override def toJsonField: JField = {

    JField(queryType.toString, JObject((
      if (must.nonEmpty)
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
      else Nil)
    ))
  }
}


case class DocumentHit(_id: String, _score: Option[Double], _source: JObject, sort: Iterable[JValue])

case class SuggestCompletionQuery(suggesterId: String, field: String, query: String)