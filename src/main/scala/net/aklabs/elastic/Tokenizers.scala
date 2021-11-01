package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._

object Tokenizers extends Enumeration {
  val standard, letter, lowercase, whitespace, uax_url_email, classic, thai,

  ngram, edge_ngram,

  keyword, pattern, simple_pattern, char_group, simple_pattern_split, path_hierarchy
  = Value
}

abstract class AbstractTokenizer(val tokenizerName: String) {
  val tokenizerType: Tokenizers.Value

  def toJsonField: JField
}
abstract class MaxLengthAbstractTokenizer(override val tokenizerName: String,
                                          val max_token_length: Option[Int]  //255
                                         ) extends AbstractTokenizer(tokenizerName) {
  override def toJsonField: JField = {
    JField(tokenizerName, JObject(JField("type", JString(tokenizerType.toString)) ::
      max_token_length.map(m => JField("max_token_length", JInt(m)) :: Nil).getOrElse(Nil)
    ))
  }
}
class StandardTokenizer(override val tokenizerName: String,
                        override val max_token_length: Option[Int] = None  //255
                       ) extends MaxLengthAbstractTokenizer(tokenizerName, max_token_length) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.standard
}
class WhitespaceTokenizer(override val tokenizerName: String,
                          override val max_token_length: Option[Int] = None  //255
                         ) extends MaxLengthAbstractTokenizer(tokenizerName, max_token_length) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.whitespace
}
class UaxUrlEmailTokenizer(override val tokenizerName: String,
                           override val max_token_length: Option[Int] = None  //255
                         ) extends MaxLengthAbstractTokenizer(tokenizerName, max_token_length) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.uax_url_email
}
class ClassicTokenizer(override val tokenizerName: String,
                       override val max_token_length: Option[Int] = None  //255
                      ) extends MaxLengthAbstractTokenizer(tokenizerName, max_token_length) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.classic
}

object CharacterClasses extends Enumeration {
  val letter, // —  for example a, b, ï or 京
  digit, // —  for example 3 or 7
  whitespace, // —  for example " " or "\n"
  punctuation, // — for example ! or "
  symbol, // —  for example $ or √
  custom // —  custom characters which need to be set using the custom_token_chars setting.
  = Value
}
abstract class NgramAbstractTokenizer(override val tokenizerName: String,
                                      val min_gram: Option[Int] = None, //1
                                      val max_gram: Option[Int] = None, //2
                                      val token_chars: Seq[CharacterClasses.Value] = Nil,
                                      val custom_token_chars: Option[String] = None
                                     ) extends AbstractTokenizer(tokenizerName) {
  override def toJsonField: JField = {
    JField(tokenizerName, JObject(JField("type", JString(tokenizerType.toString)) ::
      min_gram.map(m => JField("min_gram", JInt(m)) :: Nil).getOrElse(Nil) :::
      max_gram.map(m => JField("max_gram", JInt(m)) :: Nil).getOrElse(Nil) :::
      ( if (token_chars.isEmpty) Nil else
        JField("token_chars", JArray(token_chars.map(tch => JString(tch.toString)))) :: Nil ) :::
      custom_token_chars.map(tch => JField("custom_token_chars", JString(tch)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class NgramTokenizer(override val tokenizerName: String,
                     override val min_gram: Option[Int] = None, //1
                     override val max_gram: Option[Int] = None, //2
                     override val token_chars: Seq[CharacterClasses.Value] = Nil,
                     override val custom_token_chars: Option[String] = None
                    ) extends NgramAbstractTokenizer(tokenizerName, min_gram, max_gram, token_chars, custom_token_chars) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.ngram
}
class EdgeNgramTokenizer(override val tokenizerName: String,
                         override val min_gram: Option[Int] = None, //1
                         override val max_gram: Option[Int] = None, //2
                         override val token_chars: Seq[CharacterClasses.Value] = Nil,
                         override val custom_token_chars: Option[String] = None
                        ) extends NgramAbstractTokenizer(tokenizerName, min_gram, max_gram, token_chars, custom_token_chars) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.edge_ngram
}

object JavaRegexFlags extends Enumeration {
  val CANON_EQ, CASE_INSENSITIVE, COMMENTS, DOTALL,
  LITERAL, MULTILINE, UNICODE_CASE, UNICODE_CHARACTER_CLASS, UNIX_LINES = Value
}
class PatternTokenizer(override val tokenizerName: String,
                       pattern: Option[String] = None, // \W+
                       flags: Seq[JavaRegexFlags.Value] = Nil,
                       group: Option[Int] = None //-1
                      ) extends AbstractTokenizer(tokenizerName) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.pattern

  override def toJsonField: JField = {
    JField(tokenizerName, JObject(JField("type", JString(tokenizerType.toString)) ::
      pattern.map(p => JField("pattern", JString(p)) :: Nil).getOrElse(Nil) :::
      ( if (flags.isEmpty) Nil else JField("flags", JString(flags.map(_.toString).mkString("|"))) :: Nil ) :::
      group.map(g => JField("group", JInt(g)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class SimplePatternTokenizer(override val tokenizerName: String,
                       pattern: Option[String] = None, // \W+
                      ) extends AbstractTokenizer(tokenizerName) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.simple_pattern

  override def toJsonField: JField = {
    JField(tokenizerName, JObject(JField("type", JString(tokenizerType.toString)) ::
      pattern.map(p => JField("pattern", JString(p)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class CharacterGroupTokenizer(override val tokenizerName: String,
                              tokenize_on_chars: Seq[(Option[CharacterClasses.Value], String)]
                             ) extends AbstractTokenizer(tokenizerName) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.simple_pattern

  override def toJsonField: JField = {
    if (tokenize_on_chars.isEmpty)
      throw new Exception("tokenize on chars should be filled")
    JField(tokenizerName, JObject(JField("type", JString(tokenizerType.toString)) ::
      ( if (tokenize_on_chars.isEmpty) Nil
      else JField("tokenize_on_chars",
        JArray(tokenize_on_chars.map(ch => JString(ch._1.map(_.toString).getOrElse(ch._2))))) :: Nil )
    ))
  }
}
class SimplePatternSplitTokenizer(override val tokenizerName: String,
                             pattern: Option[String] = None, // \W+
                            ) extends AbstractTokenizer(tokenizerName) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.simple_pattern_split

  override def toJsonField: JField = {
    JField(tokenizerName, JObject(JField("type", JString(tokenizerType.toString)) ::
      pattern.map(p => JField("pattern", JString(p)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class PathHierarchyTokenizer(override val tokenizerName: String,
                             delimiter: Option[String] = None, // /
                             replacement: Option[String] = None, // delimeter
                             reverse: Option[Boolean] = None, //false
                             skip: Option[Int] = None, //0
                            ) extends AbstractTokenizer(tokenizerName) {
  override val tokenizerType: Tokenizers.Value = Tokenizers.simple_pattern_split

  override def toJsonField: JField = {
    JField(tokenizerName, JObject(JField("type", JString(tokenizerType.toString)) ::
      delimiter.map(p => JField("delimiter", JString(p)) :: Nil).getOrElse(Nil) :::
      replacement.map(p => JField("replacement", JString(p)) :: Nil).getOrElse(Nil) :::
      reverse.map(p => JField("reverse", JBool(p)) :: Nil).getOrElse(Nil) :::
      skip.map(p => JField("skip", JInt(p)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}