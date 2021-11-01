package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._
import org.pmw.tinylog.Logger

object MappingTypes extends Enumeration {
  val text, search_as_you_type, keyword,
  completion,
  `object`, nested,
  byte, short, integer, long, half_float, float, double, scaled_float,
  date, date_nanos, boolean, ip,
  geo_point, geo_shape,
  integer_range, float_range, long_range, double_range, date_range, ip_range
  = Value
}
object DateFormats extends Enumeration {
  val epoch_millis, epoch_second, date_optional_time, strict_date_optional_time,
  strict_date_optional_time_nanos,
  basic_date, basic_date_time, basic_date_time_no_millis,
  basic_ordinal_date, basic_ordinal_date_time, basic_ordinal_date_time_no_millis,
  basic_time, basic_time_no_millis,
  basic_t_time, basic_t_time_no_millis,
  basic_week_date, strict_basic_week_date,
  basic_week_date_time, strict_basic_week_date_time,
  basic_week_date_time_no_millis, strict_basic_week_date_time_no_millis,
  date, strict_date,
  date_hour, strict_date_hour,
  date_hour_minute, strict_date_hour_minute,
  date_hour_minute_second, strict_date_hour_minute_second,
  date_hour_minute_second_fraction, strict_date_hour_minute_second_fraction,
  date_hour_minute_second_millis, strict_date_hour_minute_second_millis,
  date_time, strict_date_time,
  date_time_no_millis, strict_date_time_no_millis,
  hour, strict_hour,
  hour_minute, strict_hour_minute,
  hour_minute_second, strict_hour_minute_second,
  hour_minute_second_fraction, strict_hour_minute_second_fraction,
  hour_minute_second_millis, strict_hour_minute_second_millis,
  ordinal_date, strict_ordinal_date,
  ordinal_date_time, strict_ordinal_date_time,
  ordinal_date_time_no_millis, strict_ordinal_date_time_no_millis,
  time, strict_time, time_no_millis, strict_time_no_millis, t_time, strict_t_time,
  t_time_no_millis, strict_t_time_no_millis,
  week_date, strict_week_date, week_date_time, strict_week_date_time,
  week_date_time_no_millis, strict_week_date_time_no_millis,
  weekyear, strict_weekyear, weekyear_week, strict_weekyear_week,
  weekyear_week_day, strict_weekyear_week_day,
  year, strict_year, year_month, strict_year_month, year_month_day, strict_year_month_day = Value
}
object IndexOptions extends Enumeration {
  val docs, freqs, positions, offsets = Value
}
object Similarities extends Enumeration {
  val BM25, DFR, DFI, IB, LMDirichlet, LMJelinekMercer = Value
}
object TermVectors extends Enumeration {
  val no, yes, with_positions, with_offsets, with_positions_offsets,
  with_positions_payloads, with_positions_offsets_payloads = Value
}
object SuggesterContextType extends Enumeration {
  val category, geo = Value
}

class MappingException(msg: String) extends Exception(msg)
class UpdateMappingException(msg: String) extends Exception(msg)
case class SuggesterContext(name: String,
                            `type`: SuggesterContextType.Value,
                            precision: Option[Double] = None)
case class MappingsInfo(field: String,
                        mappingType: MappingTypes.Value,
                        nested: Seq[MappingsInfo] = Nil,
                        analyzer: Option[String] = None,
                        search_analyzer: Option[String] = None,
                        search_quote_analyzer: Option[String] = None,
                        max_shingle_size: Option[Int] = None,//3
                        boost: Option[Int] = None,
                        coerce: Option[Boolean] = None, //по умолчанию true, приведение строк в числа и наоборот
                        copy_to: Seq[String] = Nil, //в какое поле копировать
                        doc_values: Option[Boolean] = None, //по умолчанию true (кроме text)
                        dynamic: Option[Boolean] = None,
                        eager_global_ordinals: Option[Boolean] = None, //по умолчанию false
                        enabled: Option[Boolean] = None, //по умолчанию true (только для технических полей)
                        fielddata: Option[Boolean] = None, //по умолчанию false, вместо doc_values используется для text
                        fields: Seq[MappingsInfo] = Nil, //multi field feature
                        format: Option[(Option[DateFormats.Value], String)] = None, //для дат
                        ignore_above: Option[Int] = None, //ограничение на длину строки превращаемой в term
                        ignore_malformed: Option[Boolean] = None, //по умолчанию false, игнорировать не правильные поля
                        index: Option[Boolean] = None, //по умолчанию true (для не технических полей)
                        index_options: Option[IndexOptions.Value] = None, //positions - по умолчанию
                        index_phrases: Option[Boolean] = None, //false, 2gram shingle
                        index_prefixes: Option[(Int, Int)] = None, //2-5 - по умолчанию
                        meta: Map[String, String] = Map.empty,
                        normalizer: Option[String] = None, //как analyzer, но для keyword (пропускаем stemming)
                        norms: Option[Boolean] = None, //по умолчанию true, отключать для полей без scoring
                        null_value: Option[Any] = None,
                        position_increment_gap: Option[Int] = None, //100, искусственное расстояние между фразами в массиве фраз
                        similarity: Option[(Option[Similarities.Value], String)] = None, //BM25
                        store: Option[Boolean] = None, //false, замена _source (вместо doc_field для text)
                        term_vector: Option[TermVectors.Value] = None, //no
                        contexts: Seq[SuggesterContext] = Nil
                       ) {
  def toJsonField(settings: AnalysisSettings): JField = {
    if (nested.nonEmpty && mappingType != MappingTypes.`object` && mappingType != MappingTypes.nested)
      throw new MappingException("Invalid mapping type with nested fields (could be 'object' or 'nested'): " + this)
    if ((analyzer.nonEmpty || search_quote_analyzer.nonEmpty) &&
      mappingType != MappingTypes.text &&
      mappingType != MappingTypes.search_as_you_type &&
      mappingType != MappingTypes.completion)
      throw new MappingException("analyzer only for text fields")
    if (normalizer.nonEmpty && mappingType != MappingTypes.keyword)
      throw new MappingException("normalizer only for keyword fields")
    if (search_analyzer.nonEmpty &&
      mappingType != MappingTypes.text &&
      mappingType != MappingTypes.search_as_you_type &&
      mappingType != MappingTypes.keyword &&
      mappingType != MappingTypes.completion)
      throw new MappingException("search_analyzer only for text or keyword fields")
    if (format.nonEmpty && mappingType != MappingTypes.date)
      throw new MappingException("format only for date fields")
    if (index_options.nonEmpty &&
      mappingType != MappingTypes.text &&
      mappingType != MappingTypes.search_as_you_type &&
      mappingType != MappingTypes.completion)
      throw new MappingException("index_options only for text fields")
    if (index_prefixes.exists(ip => ip._1 < 0 || ip._1 > ip._2) ||
      index_prefixes.exists(_._2 > 20))
      throw new MappingException("invalid min_chars or max_chars in index_prefixes")
    if (max_shingle_size.nonEmpty && mappingType != MappingTypes.search_as_you_type)
      throw new MappingException("max_shingle_size parameter only for search_as_you_type mapping type")
    if (max_shingle_size.exists(m => m < 2 || m > 4))
      throw new MappingException("max_shingle_size should be between 2-4 inclusive")

    //Logger.debug("Analysis settings: " + settings)
    val analizers = (analyzer.toList ::: search_analyzer.toList ::: search_quote_analyzer.toList).distinct
    analizers.find(a => {
      !Analizers.values.exists(_.toString == a) &&
        !settings.analyzers.exists(_.analizerName == a)
    }).foreach(a => throw new MappingException ("No analizer '%s' at index settings".format(a)))

    normalizer.foreach(n => {
      if (!Normalizers.values.exists(_.toString == n) &&
        !settings.normalizers.exists(_.normalizerName == n))
        throw new MappingException ("No normalizer '%s' at index settings".format(n))
    })


    JField(field, JObject(
      JField("type", JString(mappingType.toString)) ::
        (if (nested.isEmpty) Nil else JField("properties", JObject(nested.map(_.toJsonField(settings)))) :: Nil) :::
        analyzer.map(a => JField("analyzer", JString(a)) :: Nil).getOrElse(Nil) :::
        search_analyzer.map(a => JField("search_analyzer", JString(a)) :: Nil).getOrElse(Nil) :::
        search_quote_analyzer.map(a => JField("search_quote_analyzer", JString(a)) :: Nil).getOrElse(Nil) :::
        boost.map(b => JField("boost", JInt(b)) :: Nil).getOrElse(Nil) :::
        coerce.map(c => JField("coerce", JBool(c)) :: Nil).getOrElse(Nil) :::
        (if (copy_to.isEmpty) Nil else JField("copy_to", JArray(copy_to.map(JString(_)))) :: Nil) :::
        doc_values.map(dv => JField("doc_values", JBool(dv)) :: Nil).getOrElse(Nil) :::
        dynamic.map(d => JField("dynamic", JBool(d)) :: Nil).getOrElse(Nil) :::
        eager_global_ordinals.map(d => JField("eager_global_ordinals", JBool(d)) :: Nil).getOrElse(Nil) :::
        // enabled - только технические поля
        fielddata.map(fd => JField("fielddata", JBool(fd)) :: Nil).getOrElse(Nil) :::
        (if (fields.isEmpty) Nil else JField("fields", JObject(fields.map(_.toJsonField(settings)))) :: Nil) :::
        format.map(f => JField("format", JString(f._1.map(_.toString).getOrElse(f._2))) :: Nil).getOrElse(Nil) :::
        ignore_above.map(i => JField("ignore_above", JInt(i)) :: Nil).getOrElse(Nil) :::
        ignore_malformed.map(i => JField("ignore_malformed", JBool(i)) :: Nil).getOrElse(Nil) :::
        index.map(i => JField("index", JBool(i)) :: Nil).getOrElse(Nil) :::
        index_options.map(i => JField("index_options", JString(i.toString)) :: Nil).getOrElse(Nil) :::
        index_phrases.map(i => JField("index_phrases", JBool(i)) :: Nil).getOrElse(Nil) :::
        index_prefixes.map(i => JField("index_prefixes",
          JObject(JField("min_chars", JInt(i._1)) :: JField("max_chars", JInt(i._2)) :: Nil)) :: Nil).getOrElse(Nil) :::
        (if (meta.isEmpty) Nil else JField("meta", JObject(meta.toSeq.map(kv => JField(kv._1, JString(kv._2))))) :: Nil) :::
        normalizer.map(n => JField("normalizer", JString(n)) :: Nil).getOrElse(Nil) :::
        norms.map(n => JField("norms", JString(n.toString)) :: Nil).getOrElse(Nil) :::
        null_value.map(nv => JField("norms", any2JValue(nv, "null_value")) :: Nil).getOrElse(Nil) :::
        position_increment_gap.map(pig => JField("position_increment_gap", JInt(pig)) :: Nil).getOrElse(Nil) :::
        similarity.map(n => JField("similarity", JString(n._1.map(_.toString).getOrElse(n._2))) :: Nil).getOrElse(Nil) :::
        store.map(s => JField("store", JBool(s)) :: Nil).getOrElse(Nil) :::
        term_vector.map(t => JField("store", JString(t.toString)) :: Nil).getOrElse(Nil) :::
        (if (contexts.isEmpty) Nil else JField("contexts", JArray(contexts.map(c => {
          if (c.precision.nonEmpty && c.`type` != SuggesterContextType.geo)
            throw new Exception("There can not be precition in context type category (only for geo)")
          JObject(
            JField("name", JString(c.name)) ::
            JField("type", JString(c.`type`.toString)) ::
              c.precision.map(p => JField("precision", JDouble(p)) :: Nil).getOrElse(Nil)
          )
        }))) :: Nil) ::: Nil
    ))
  }

  def changes_?(mi: MappingsInfo): Boolean = {
    if (field != mi.field || mappingType != mi.mappingType)
      throw new Exception("wrong field name or type")

    if (nested.exists(n => {
      mi.nested.find(_.field == n.field).forall(mi_nested => {
        n.changes_?(mi_nested)
      })
    }) || (analyzer.nonEmpty && analyzer != mi.analyzer) ||
      {
        val sa_old = if (mi.search_analyzer == mi.analyzer) None else mi.search_analyzer
        val sa_new = if (search_analyzer == analyzer) None else search_analyzer

        sa_new.nonEmpty && sa_new != sa_old
      } ||
      {
        val sq_old = if (mi.search_quote_analyzer == mi.analyzer) None else mi.search_quote_analyzer
        val sq_new = if (search_quote_analyzer == analyzer) None else search_quote_analyzer

        sq_new.nonEmpty && sq_new != sq_old
      } ||
      //(search_analyzer.nonEmpty && search_analyzer != mi.search_analyzer) ||
      //(search_quote_analyzer.nonEmpty && search_quote_analyzer != mi.search_quote_analyzer) ||
      (max_shingle_size.nonEmpty && max_shingle_size != mi.max_shingle_size) ||
      (boost.nonEmpty && boost != mi.boost) ||
      (coerce.nonEmpty && coerce != mi.coerce) ||
      (copy_to.nonEmpty && copy_to != mi.copy_to) ||
      (doc_values.nonEmpty && doc_values != mi.doc_values) ||
      (dynamic.nonEmpty && dynamic != mi.dynamic) ||
      (eager_global_ordinals.nonEmpty && eager_global_ordinals != mi.eager_global_ordinals) ||
      (enabled.nonEmpty && enabled != mi.enabled) ||
      (fielddata.nonEmpty && fielddata != mi.fielddata) ||
      fields.exists(n => {
        mi.fields.find(_.field == n.field).forall(mi_field => {
          n.changes_?(mi_field)
        })
      }) ||
      (format.nonEmpty && format != mi.format) ||
      (ignore_above.nonEmpty && ignore_above != mi.ignore_above) ||
      (ignore_malformed.nonEmpty && ignore_malformed != mi.ignore_malformed) ||
      (index.nonEmpty && index != mi.index) ||
      (index_options.nonEmpty && index_options != mi.index_options) ||
      (index_phrases.nonEmpty && index_phrases != mi.index_phrases) ||
      (index_prefixes.nonEmpty && index_prefixes != mi.index_prefixes) ||
      (meta.nonEmpty && meta != mi.meta) ||
      (normalizer.nonEmpty && normalizer != mi.normalizer) ||
      (norms.nonEmpty && norms != mi.norms) ||
      (null_value.nonEmpty && null_value != mi.null_value) ||
      (position_increment_gap.nonEmpty && position_increment_gap != mi.position_increment_gap) ||
      (similarity.nonEmpty && similarity != mi.similarity) ||
      (store.nonEmpty && store != mi.store) ||
      (term_vector.nonEmpty && term_vector != mi.term_vector)) true
    else false
  }
}
