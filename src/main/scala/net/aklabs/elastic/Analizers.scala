package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._

object Analizers extends Enumeration {
  val standard, simple, whitespace, stop, keyword, pattern,

  arabic, armenian, basque, bengali, brazilian, bulgarian, catalan, cjk, czech,
  danish, dutch, english, estonian, finnish, french, galician, german, greek,
  hindi, hungarian, indonesian, irish, italian, latvian, lithuanian, norwegian,
  persian, portuguese, romanian, russian, sorani, spanish, swedish, turkish, thai,

  fingerprint, custom = Value

  val no_stem_exclusion: Set[Analizers.Value] = Set(brazilian, cjk, danish, estonian, greek, persian, thai)
}
object LanguageAnalizers extends Enumeration {
  val arabic, armenian, basque, bengali, brazilian, bulgarian, catalan, cjk, czech,
  danish, dutch, english, estonian, finnish, french, galician, german, greek,
  hindi, hungarian, indonesian, irish, italian, latvian, lithuanian, norwegian,
  persian, portuguese, romanian, russian, sorani, spanish, swedish, turkish, thai = Value
}

abstract class AbstractAnalizer(val analizerName: String) {
  val analizerType: Analizers.Value

  def toJsonField: JField
}
class CustomAnalizer(override val analizerName: String,
                     tokenizer: (Option[Tokenizers.Value], String),
                     char_filter: Seq[(Option[CharacterFilters.Value], String)] = Nil,
                     filter: Seq[(Option[TokenFilters.Value], String)] = Nil,
                     position_increment_gap: Option[Int] = None //100
                    ) extends AbstractAnalizer(analizerName) {
  override val analizerType: Analizers.Value = Analizers.custom

  def toJsonField: JField = {
    if (tokenizer._1.isEmpty && tokenizer._2.isEmpty)
      throw new Exception("tokenizer should not be empty")
    JField(analizerName, JObject(JField("type", JString(analizerType.toString)) ::
      JField("tokenizer", JString( tokenizer._1.map(_.toString).getOrElse(tokenizer._2) )) ::
      ( if (char_filter.isEmpty) Nil else
        JField("char_filter", JArray(char_filter.map(cf =>
          JString( cf._1.map(_.toString).getOrElse(cf._2) )
        ))) :: Nil ) :::
      ( if (filter.isEmpty) Nil else
        JField("filter", JArray(filter.map(f =>
          JString( f._1.map(_.toString).getOrElse(f._2) )
        ))) :: Nil ) :::
      position_increment_gap.map(pi => JField("position_increment_gap", JInt(pi)) :: Nil).getOrElse(Nil)
    ))
  }
}

class StandardAnalizer(override val analizerName: String,
                       stopwords: Option[StopWordsByLanguage.Value] = None, //_english_
                       stopwords_list: Seq[String] = Nil,
                       stopwords_path: Option[String] = None,
                       max_token_length: Option[Int] = None //255
                     ) extends AbstractAnalizer(analizerName) {
  override val analizerType: Analizers.Value = Analizers.standard

  override def toJsonField: JField = {
    JField(analizerName, JObject(JField("type", JString(analizerType.toString)) ::
      stopwords.map(s => JField("stopwords", JString(s.toString)) :: Nil)
        .getOrElse(if (stopwords_list.isEmpty) Nil
        else JField("stopwords", JArray(stopwords_list.map(JString(_)))) :: Nil) :::
      stopwords_path.map(s => JField("stopwords_path", JString(s)) :: Nil).getOrElse(Nil) :::
      max_token_length.map(m => JField("max_token_length", JInt(m)) :: Nil).getOrElse(Nil) :::
      Nil))
  }
}
class StopAnalizer(override val analizerName: String,
                   stopwords: Option[StopWordsByLanguage.Value] = None, //_english_
                   stopwords_list: Seq[String] = Nil,
                   stopwords_path: Option[String] = None
                  ) extends AbstractAnalizer(analizerName) {
  override val analizerType: Analizers.Value = Analizers.stop

  override def toJsonField: JField = {
    JField(analizerName, JObject(JField("type", JString(analizerType.toString)) ::
      stopwords.map(s => JField("stopwords", JString(s.toString)) :: Nil)
        .getOrElse(if (stopwords_list.isEmpty) Nil
        else JField("stopwords", JArray(stopwords_list.map(JString(_)))) :: Nil) :::
      stopwords_path.map(s => JField("stopwords_path", JString(s)) :: Nil).getOrElse(Nil) :::
      Nil))
  }
}
class PatternAnalizer(override val analizerName: String,
                      pattern: Option[String] = None, // \W+
                      flags: Seq[JavaRegexFlags.Value] = Nil,
                      lowercase: Option[Boolean] = None, //true
                      stopwords: Option[StopWordsByLanguage.Value] = None, //_none_
                      stopwords_list: Seq[String] = Nil,
                      stopwords_path: Option[String] = None
                     ) extends AbstractAnalizer(analizerName) {
  override val analizerType: Analizers.Value = Analizers.pattern

  override def toJsonField: JField = {
    JField(analizerName, JObject(JField("type", JString(analizerType.toString)) ::
      stopwords.map(s => JField("stopwords", JString(s.toString)) :: Nil)
        .getOrElse(if (stopwords_list.isEmpty) Nil
        else JField("stopwords", JArray(stopwords_list.map(JString(_)))) :: Nil) :::
      stopwords_path.map(s => JField("stopwords_path", JString(s)) :: Nil).getOrElse(Nil) :::
      pattern.map(p => JField("pattern", JString(p)) :: Nil).getOrElse(Nil) :::
      ( if (flags.isEmpty) Nil else JField("flags", JString(flags.map(_.toString).mkString("|"))) :: Nil ) :::
      lowercase.map(l => JField("lowercase", JBool(l)) :: Nil).getOrElse(Nil) :::
      Nil))
  }
}

class LanguageAnalizers(override val analizerName: String,
                        language: LanguageAnalizers.Value, //
                        stem_exclusion: Seq[String] = Nil,
                        stopwords: Option[StopWordsByLanguage.Value] = None, //_none_
                        stopwords_list: Seq[String] = Nil,
                        stopwords_path: Option[String] = None
                       ) extends AbstractAnalizer(analizerName) {
  override val analizerType: Analizers.Value = Analizers.withName(language.toString)

  override def toJsonField: JField = {
    if (stem_exclusion.nonEmpty && Analizers.no_stem_exclusion.contains(analizerType))
      throw new Exception("stem_exclusion is not supported for %s".format(analizerType.toString))
    JField(analizerName, JObject(JField("type", JString(analizerType.toString)) ::
      stopwords.map(s => JField("stopwords", JString(s.toString)) :: Nil)
        .getOrElse(if (stopwords_list.isEmpty) Nil
        else JField("stopwords", JArray(stopwords_list.map(JString(_)))) :: Nil) :::
      ( if (stem_exclusion.isEmpty) Nil
      else JField("stem_exclusion", JArray(stem_exclusion.map(JString(_)))) :: Nil ) :::
      stopwords_path.map(s => JField("stopwords_path", JString(s)) :: Nil).getOrElse(Nil) :::
      Nil))
  }
}

class FingerprintAnalizer(override val analizerName: String,
                          separator: Option[String] = None, // space - to concatenate
                          max_output_size: Option[Int] = None, //255
                          stopwords: Option[StopWordsByLanguage.Value] = None, //_none_
                          stopwords_list: Seq[String] = Nil,
                          stopwords_path: Option[String] = None
                         ) extends AbstractAnalizer(analizerName) {
  override val analizerType: Analizers.Value = Analizers.fingerprint

  override def toJsonField: JField = {
    JField(analizerName, JObject(JField("type", JString(analizerType.toString)) ::
      stopwords.map(s => JField("stopwords", JString(s.toString)) :: Nil)
        .getOrElse(if (stopwords_list.isEmpty) Nil
        else JField("stopwords", JArray(stopwords_list.map(JString(_)))) :: Nil) :::
      stopwords_path.map(s => JField("stopwords_path", JString(s)) :: Nil).getOrElse(Nil) :::
      separator.map(p => JField("separator", JString(p)) :: Nil).getOrElse(Nil) :::
      max_output_size.map(l => JField("max_output_size", JInt(l)) :: Nil).getOrElse(Nil) :::
      Nil))
  }
}