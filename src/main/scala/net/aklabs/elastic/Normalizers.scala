package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._

object Normalizers extends Enumeration {
  val arabic_normalization, asciifolding, bengali_normalization,
  cjk_width, decimal_digit, elision, german_normalization, hindi_normalization,
  indic_normalization, lowercase, persian_normalization, scandinavian_folding,
  serbian_normalization, sorani_normalization, uppercase, custom
  = Value
}
case class CustomNormalizer(normalizerName: String,
                            char_filter: Seq[(Option[CharacterFilters.Value], String)],
                            filter: Seq[(Option[TokenFilters.Value], String)]) {
  def toJsonField: JField = {
    if (char_filter.isEmpty && filter.isEmpty)
      throw new Exception("char_filter or filter should not be empty")
    JField(normalizerName, JObject(JField("type", JString(Normalizers.custom.toString)) ::
      ( if (char_filter.isEmpty) Nil else
        JField("char_filter", JArray(char_filter.map(cf =>
          JString( cf._1.map(_.toString).getOrElse(cf._2) )
        ))) :: Nil ) :::
      ( if (filter.isEmpty) Nil else
      JField("filter", JArray(filter.map(f =>
        JString( f._1.map(_.toString).getOrElse(f._2) )
      ))) :: Nil ) ::: Nil
    ))
  }
}
