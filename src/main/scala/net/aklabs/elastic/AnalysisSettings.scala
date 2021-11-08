package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers.{JField, JObject}

case class AnalysisSettings(charFilters: Seq[AbstractCharacterFilter] = Nil,
                            tokenizers: Seq[AbstractTokenizer] = Nil,
                            filters: Seq[AbstractTokenFilter] = Nil,
                            normalizers: Seq[CustomNormalizer] = Nil,
                            analyzers: Seq[AbstractAnalizer] = Nil) {
  def toJsonField: JField = {
    val analysisJFields =
      (if (charFilters.isEmpty) Nil else JField("char_filter", JObject(charFilters.map(_.toJsonField))) :: Nil) :::
        (if (tokenizers.isEmpty) Nil else JField("tokenizer", JObject(tokenizers.map(_.toJsonField))) :: Nil) :::
        (if (filters.isEmpty) Nil else JField("filter", JObject(filters.map(_.toJsonField))) :: Nil) :::
        (if (normalizers.isEmpty) Nil else JField("normalizer", JObject(normalizers.map(_.toJsonField))) :: Nil) :::
        (if (analyzers.isEmpty) Nil else JField("analyzer", JObject(analyzers.map(_.toJsonField))) :: Nil)

    JField("analysis", JObject(analysisJFields))
  }
}

object SortingType extends Enumeration {
  val asc, desc = Value
}