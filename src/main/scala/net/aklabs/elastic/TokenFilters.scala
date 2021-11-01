package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._

object TokenFilters extends Enumeration {
  val apostrophe, asciifolding, cjk_bigram, cjk_width,
  classic, common_grams, condition, decimal_digit, delimited_payload, dictionary_decompounder,
  edge_ngram, elision, fingerprint,
  flatten_graph, //lossy process, use other graphs at analizers
  hunspell, //hunspell dictionary for stemming
  hyphenation_decompounder, //изодногодлинногослова вычленяет слова, сделано изначально для германских языков
  keep_types, keep,
  keyword_marker, //не приводит ключевые слова к корням
  keyword_repeat, //каждое слово дублирует как keyword, используется в комбинации со stemming'ом
  length, limit, lowercase,
  min_hash, //для поиска похожих
  multiplexer, //каждый токен генерируется по несколько раз из списка фильтров
  ngram,
  arabic_normalization, german_normalization, hindi_normalization, indic_normalization,
  sorani_normalization, persian_normalization, scandinavian_normalization, scandinavian_folding,
  serbian_normalization,
  pattern_capture, //извлекает токены по группам регулярного выражения
  pattern_replace, //заменяет по регулярному выражению
  porter_stem, //stemmer но только для английского языка
  predicate_token_filter, remove_duplicates, reverse,
  shingle, //полезно для поика по фразам
  snowball, //snowball stemmer
  stemmer, //алгоритмический stemmer
  stemmer_override,
  stop, //stop-words
  synonym, synonym_graph,
  trim, truncate, unique, uppercase,
  word_delimiter, word_delimiter_graph,

  icu_transform, phonetic
  = Value
}
abstract class AbstractTokenFilter(val filterName: String) {
  val filterType: TokenFilters.Value

  def toJsonField: JField
}
class ASCIIFoldingTokenFilter(override val filterName: String,
                              preserve_original: Boolean
                             ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.asciifolding

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("preserve_original", JBool(preserve_original)) :: Nil))
  }
}
object CJKScripts extends Enumeration {
  val han, hangul, hiragana, katakana = Value
}
class CJKBigramTokenFilter(override val filterName: String,
                      ignored_scripts: Seq[CJKScripts.Value] = Nil,
                      output_unigrams: Option[Boolean] = None //false
                     ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.cjk_bigram

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      (if (ignored_scripts.isEmpty) Nil else
        JField("ignored_scripts", JArray(ignored_scripts.map(s => JString(s.toString)))) :: Nil) :::
      output_unigrams.map(o => JField("output_unigrams", JBool(o)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class CommonGramsTokenFilter(override val filterName: String,
                        common_words: Seq[String] = Nil,
                        common_words_path: Option[String] = None,
                        ignore_case: Option[Boolean] = None, //false
                        query_mode: Option[Boolean] = None //false, нужно ставить true, чтобы убрать common_words из биграмм, рекомендовано для search analyzer'ов
                       ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.common_grams

  override def toJsonField: JField = {
    if (common_words.isEmpty && common_words_path.isEmpty)
      throw new Exception("common_words or common_words_path is required")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      (if (common_words.nonEmpty) JField("common_words", JArray(common_words.map(JString(_)))) else
        JField("common_words_path", JString(common_words_path.get))) ::
      ignore_case.map(i => JField("ignore_case", JBool(i)) :: Nil).getOrElse(Nil) :::
      query_mode.map(q => JField("query_mode", JBool(q)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class ConditionalTokenFilter(override val filterName: String,
                             filter: Seq[(Option[TokenFilters.Value], String)],
                             script: InlineScript
                            ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.condition

  override def toJsonField: JField = {
    if (filter.isEmpty)
      throw new Exception("filter cannot be empty")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("filter", JArray(filter.map(f => JString(f._1.map(_.toString).getOrElse(f._2))))) ::
      script.toJsonField :: Nil
    ))
  }
}
object PayloadEncodings extends Enumeration {
  val float, identity, int = Value
}
class DelimitedPayloadTokenFilter(override val filterName: String,
                                  delimiter: Option[String], //|
                                  encoding: Option[PayloadEncodings.Value]
                                 ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.delimited_payload

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      delimiter.map(d => JField("delimiter", JString(d)) :: Nil).getOrElse(Nil) :::
      encoding.map(e => JField("encoding", JString(e.toString)) :: Nil).getOrElse(Nil)
    ))
  }
}
class DictionaryDecompounderTokenFilter(override val filterName: String,
                                        word_list: Seq[String] = Nil,
                                        word_list_path: Option[String] = None,
                                        max_subword_size: Option[Int] = None, //15
                                        min_subword_size: Option[Int] = None, //2
                                        min_word_size: Option[Int] = None, //5
                                        only_longest_match: Option[Boolean]
                                       ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.dictionary_decompounder

  override def toJsonField: JField = {
    if (word_list.isEmpty && word_list_path.isEmpty)
      throw new Exception("common_words or common_words_path is required")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      (if (word_list.nonEmpty) JField("word_list", JArray(word_list.map(JString(_)))) else
        JField("word_list_path", JString(word_list_path.get))) ::
      max_subword_size.map(m => JField("max_subword_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      min_subword_size.map(m => JField("min_subword_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      min_word_size.map(m => JField("min_word_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      only_longest_match.map(o => JField("only_longest_match", JBool(o)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class EdgeNgramTokenFilter(override val filterName: String,
                      max_gram: Option[Int] = None, //2
                      min_gram: Option[Int] = None, //1
                      preserve_original: Option[Boolean] = None, //false
                      ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.edge_ngram

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      max_gram.map(m => JField("max_gram", JInt(m)) :: Nil).getOrElse(Nil) :::
      min_gram.map(m => JField("min_gram", JInt(m)) :: Nil).getOrElse(Nil) :::
      preserve_original.map(o => JField("preserve_original", JBool(o)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class ElisionTokenFilter(override val filterName: String,
                    articles: Seq[String] = Nil,
                    articles_path: Option[String] = None,
                    articles_case: Option[Boolean] //false - case sensitive
                   ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.elision

  override def toJsonField: JField = {
    if (articles.isEmpty && articles_path.isEmpty)
      throw new Exception("articles or articles_path is required")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      (if (articles.nonEmpty) JField("articles", JArray(articles.map(JString(_)))) else
        JField("articles_path", JString(articles_path.get))) ::
      articles_case.map(ac => JField("articles_case", JBool(ac)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class FingerprintTokenFilter(override val filterName: String,
                        max_output_size: Option[Int] = None, //255
                        separator: Option[String] = None //space
                       ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.fingerprint

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      max_output_size.map(o => JField("articles_case", JInt(o)) :: Nil).getOrElse(Nil) :::
      separator.map(s => JField("separator", JString(s)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class HunspellStemTokenFilter(override val filterName: String,
                         language: String, //дириктория с *.aff и *.dic
                         dictionary: Seq[String] = Nil,
                         dedup: Option[Boolean] = None, //true, убирает дубликаты
                         longest_only: Option[Boolean] = None //false
                        ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.hunspell

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("language", JString(language)) ::
      (if (dictionary.isEmpty) Nil else JField("dictionary", JArray(dictionary.map(JString(_)))) :: Nil) :::
      dedup.map(d => JField("dedup", JBool(d)) :: Nil).getOrElse(Nil) :::
      longest_only.map(l => JField("longest_only", JBool(l)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class HyphenationDecompunderTokenFilter(override val filterName: String,
                                   hyphenation_patterns_path: String,
                                   word_list: Seq[String] = Nil,
                                   word_list_path: Option[String] = None,
                                   max_subword_size: Option[Int] = None, //15
                                   min_subword_size: Option[Int] = None, //2
                                   min_word_size: Option[Int] = None, //5
                                   only_longest_match: Option[Boolean]
                                  ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.hyphenation_decompounder

  override def toJsonField: JField = {
    if (word_list.isEmpty && word_list_path.isEmpty)
      throw new Exception("common_words or common_words_path is required")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("hyphenation_patterns_path", JString(hyphenation_patterns_path)) ::
      (if (word_list.nonEmpty) JField("word_list", JArray(word_list.map(JString(_)))) else
        JField("word_list_path", JString(word_list_path.get))) ::
      max_subword_size.map(m => JField("max_subword_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      min_subword_size.map(m => JField("min_subword_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      min_word_size.map(m => JField("min_word_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      only_longest_match.map(o => JField("only_longest_match", JBool(o)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
object TokenTypes extends Enumeration {
  val NUM, ALPHANUM, HANGUL, word, SYNONYM = Value
}
object KeepTypesMode extends Enumeration {
  val include, exclude = Value
}
class KeepTypesTokenFilter(override val filterName: String,
                      types: Seq[TokenTypes.Value],
                      mode: Option[KeepTypesMode.Value] = None //include
                     ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.keep_types

  override def toJsonField: JField = {
    if (types.isEmpty)
      throw new Exception("types is required")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("types", JArray(types.map(t => JString("<%s>".format(t.toString))))) ::
      mode.map(m => JField("mode", JString(m.toString)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class KeepTokenFilter(override val filterName: String,
                 keep_words: Seq[String] = Nil,
                 keep_words_path: Option[String] = None,
                 keep_words_case: Option[Boolean] //false - case sensitive
                ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.keep

  override def toJsonField: JField = {
    if (keep_words.isEmpty && keep_words_path.isEmpty)
      throw new Exception("keep_words or keep_words_path is required")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      (if (keep_words.nonEmpty) JField("keep_words", JArray(keep_words.map(JString(_)))) else
        JField("keep_words_path", JString(keep_words_path.get))) ::
      keep_words_case.map(c => JField("keep_words_case", JBool(c)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class KeywordMarkerTokenFilter(override val filterName: String,
                          keywords: Seq[String] = Nil,
                          keywords_path: Option[String] = None,
                          keywords_pattern: Option[String] = None,
                          ignore_case: Option[Boolean] //false
                ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.keyword_marker

  override def toJsonField: JField = {
    if (keywords.isEmpty && keywords_path.isEmpty && keywords_pattern.isEmpty)
      throw new Exception("keep_words or keep_words_path is required")
    if (keywords_pattern.nonEmpty && (keywords.nonEmpty || keywords_path.nonEmpty))
      throw new Exception("if keywords_pattern specified keywords and keywords_path should be empty")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      (if (keywords.nonEmpty) JField("keywords", JArray(keywords.map(JString(_)))) :: Nil else Nil) :::
      keywords_path.map(p => JField("keywords_path", JString(p)) :: Nil).getOrElse(Nil) :::
      keywords_pattern.map(p => JField("keywords_pattern", JString(p)) :: Nil).getOrElse(Nil) :::
      ignore_case.map(c => JField("ignore_case", JBool(c)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class LengthTokenFilter(override val filterName: String,
                   min: Option[Int] = None, //0
                   max: Option[Int] = None //Integer.MAX_VALUE, which is 2^31-1 or 2147483647
                  ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.length

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      min.map(m => JField("min", JInt(m)) :: Nil).getOrElse(Nil) :::
      max.map(m => JField("max", JInt(m)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class LimitTokenCountFilter(override val filterName: String,
                            max_token_count: Option[Int] = None, //1
                            consume_all_tokens: Option[Boolean] = None //false
                           ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.limit

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      max_token_count.map(m => JField("max_token_count", JInt(m)) :: Nil).getOrElse(Nil) :::
      consume_all_tokens.map(all => JField("consume_all_tokens", JBool(all)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class LowercaseTokenFilter(override val filterName: String,
                           language: Option[String] = None
                          ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.lowercase

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      language.map(l => JField("language", JString(l)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class MinHashTokenFilter(override val filterName: String,
                         bucket_count: Option[Int] = None, //512
                         hash_count: Option[Int] = None, //1
                         hash_set_size: Option[Int] = None, //1
                         with_rotation: Option[Boolean] = None //false
                        ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.min_hash

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      bucket_count.map(b => JField("bucket_count", JInt(b)) :: Nil).getOrElse(Nil) :::
      hash_count.map(h => JField("hash_count", JInt(h)) :: Nil).getOrElse(Nil) :::
      hash_set_size.map(h => JField("hash_set_size", JInt(h)) :: Nil).getOrElse(Nil) :::
      with_rotation.map(w => JField("with_rotation", JBool(w)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class MultiplexerTokenFilter(override val filterName: String,
                             filters: Seq[(Option[TokenFilters.Value], String)],
                             preserve_original: Option[Boolean] = None //true
                            ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.multiplexer

  override def toJsonField: JField = {
    if (filters.isEmpty)
      throw new Exception("filters can not be empty")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("filters", JArray(filters.map(f => f._1.map(f => JString(f.toString)).getOrElse(JString(f._2))))) ::
      preserve_original.map(w => JField("preserve_original", JBool(w)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class NgramTokenFilter(override val filterName: String,
                       max_gram: Option[Int] = None, //2
                       min_gram: Option[Int] = None, //1
                       preserve_original: Option[Boolean] = None //false
                      ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.ngram

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      max_gram.map(m => JField("max_gram", JInt(m)) :: Nil).getOrElse(Nil) :::
      min_gram.map(m => JField("min_gram", JInt(m)) :: Nil).getOrElse(Nil) :::
      preserve_original.map(p => JField("preserve_original", JBool(p)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class PatternCaptureTokenFilter(override val filterName: String,
                                patterns: Seq[String],
                                preserve_original: Option[Boolean] = None //true
                               ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.pattern_capture

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("patterns", JArray(patterns.map(JString(_)))) ::
      preserve_original.map(p => JField("preserve_original", JBool(p)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class PatternReplaceTokenFilter(override val filterName: String,
                                pattern: String,
                                replacement: Option[String] = None, //""
                                all: Option[Boolean] = None //true
                               ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.pattern_replace

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("pattern", JString(pattern)) ::
      replacement.map(r => JField("replacement", JString(r)) :: Nil).getOrElse(Nil) :::
      all.map(a => JField("all", JBool(a)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class PredicateTokenFilter(override val filterName: String,
                           script: InlinePainlessScript
                          ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.predicate_token_filter

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      script.toJsonField :: Nil
    ))
  }
}
class ShingleTokenFilter(override val filterName: String,
                         min_shingle_size: Option[Int] = None, //2
                         max_shingle_size: Option[Int] = None, //1
                         output_unigrams: Option[Boolean] = None //true
                      ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.shingle

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      min_shingle_size.map(m => JField("min_shingle_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      max_shingle_size.map(m => JField("max_shingle_size", JInt(m)) :: Nil).getOrElse(Nil) :::
      output_unigrams.map(o => JField("output_unigrams", JBool(o)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
object SnowballLanguages extends Enumeration {
  val Armenian, Basque, Catalan, Danish, Dutch, English, Finnish, French,
  German, German2, Hungarian, Italian, Kp, Lithuanian, Lovins, Norwegian,
  Porter, Portuguese, Romanian, Russian, Spanish, Swedish, Turkish = Value
}
class SnowballTokenFilter(override val filterName: String,
                          language: SnowballLanguages.Value
                         ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.snowball

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("language", JString(language.toString)) :: Nil
    ))
  }
}
object StemmerLanguages extends Enumeration {
  val arabic, armenian, basque, bengali, brazilian, bulgarian, catalan, czech, danish,
  dutch, dutch_kp,
  english, light_english, lovins, minimal_english, porter2, possessive_english,
  estonian,
  finnish, light_finnish,
  light_french, french, minimal_french,
  galician, minimal_galician,
  light_german, german, german2, minimal_german,
  greek, hindi,
  hungarian, light_hungarian,
  indonesian, irish,
  light_italian, italian,
  sorani, latvian, lithuanian,
  norwegian, light_norwegian, minimal_norwegian,
  light_nynorsk, minimal_nynorsk,
  light_portuguese, minimal_portuguese, portuguese, portuguese_rslp,
  romanian,
  russian, light_russian,
  light_spanish, spanish,
  swedish, light_swedish,
  turkish = Value
}
class StemmerTokenFilter(override val filterName: String,
                          language: StemmerLanguages.Value
                         ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.stemmer

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("language", JString(language.toString)) :: Nil
    ))
  }
}
class StemmerOverrideTokenFilter(override val filterName: String,
                                 rules: Seq[String] = Nil,
                                 rules_path: Option[String] = None
                                ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.stemmer_override

  override def toJsonField: JField = {
    if (rules.isEmpty && rules_path.isEmpty)
      throw new Exception("rules or rules_path should not be empty")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      ( if (rules.isEmpty) JField("rules_path", JString(rules_path.get)) else
      JField("rules", JArray(rules.map(JString(_)))) ) :: Nil
    ))
  }
}
object StopWordsByLanguage extends Enumeration {
  val _arabic_, _armenian_, _basque_, _bengali_, _brazilian_, _bulgarian_,
  _catalan_, _cjk_, _czech_, _danish_, _dutch_, _english_, _estonian_,
  _finnish_, _french_, _galician_, _german_, _greek_, _hindi_, _hungarian_,
  _indonesian_, _irish_, _italian_, _latvian_, _lithuanian_, _norwegian_,
  _persian_, _portuguese_, _romanian_, _russian_, _sorani_, _spanish_,
  _swedish_, _thai_, _turkish_ = Value
}
class StopWordsFilter(override val filterName: String,
                      stopwords: Option[StopWordsByLanguage.Value] = None, //_english_
                      stopwords_list: Seq[String] = Nil,
                      stopwords_path: Option[String] = None,
                      ignore_case: Option[Boolean] = None, //false
                      remove_trailing: Option[Boolean] = None //true, нужно убрать для completion suggester'а
                     ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.stop

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      stopwords.map(s => JField("stopwords", JString(s.toString)) :: Nil)
        .getOrElse(if (stopwords_list.isEmpty) Nil
        else JField("stopwords", JArray(stopwords_list.map(JString(_)))) :: Nil) :::
      stopwords_path.map(s => JField("stopwords_path", JString(s)) :: Nil).getOrElse(Nil) :::
      ignore_case.map(i => JField("ignore_case", JBool(i)) :: Nil).getOrElse(Nil) :::
      remove_trailing.map(r => JField("remove_trailing", JBool(r)) :: Nil).getOrElse(Nil) :::
      Nil))
  }
}
class SynonimTokenFilter(override val filterName: String,
                         synonyms: Seq[String] = Nil,
                         synonyms_path: Option[String] = None,
                         expand: Option[Boolean] = None, //true
                         lenient: Option[Boolean] = None //false
                        ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.synonym

  override def toJsonField: JField = {
    if (synonyms.isEmpty && synonyms_path.isEmpty)
      throw new Exception("synonyms or synonyms_path should not be empty")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      ( if (synonyms.isEmpty) JField("synonyms_path", JString(synonyms_path.get)) else
        JField("synonyms", JArray(synonyms.map(JString(_)))) ) ::
      expand.map(e => JField("expand", JBool(e)) :: Nil).getOrElse(Nil) :::
      lenient.map(l => JField("lenient", JBool(l)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class SynonymGraphTokenFilter(override val filterName: String,
                              synonyms: Seq[String] = Nil,
                              synonyms_path: Option[String] = None,
                              expand: Option[Boolean] = None, //true
                              lenient: Option[Boolean] = None //false
                        ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.synonym_graph

  override def toJsonField: JField = {
    if (synonyms.isEmpty && synonyms_path.isEmpty)
      throw new Exception("synonyms or synonyms_path should not be empty")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      ( if (synonyms.isEmpty) JField("synonyms_path", JString(synonyms_path.get)) else
        JField("synonyms", JArray(synonyms.map(JString(_)))) ) ::
      expand.map(e => JField("expand", JBool(e)) :: Nil).getOrElse(Nil) :::
      lenient.map(l => JField("lenient", JBool(l)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
class TruncateTokenFilter(override val filterName: String,
                          length: Option[Int] //10
                         ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.truncate

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      length.map(l => JField("lenient", JInt(l)) :: Nil).getOrElse(Nil)
    ))
  }
}
class UniqueTokenFilter(override val filterName: String,
                        only_on_same_position: Option[Boolean] //false, when true, the unique filter works the same as remove_duplicates filter.
                       ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.unique

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      only_on_same_position.map(sp => JField("lenient", JBool(sp)) :: Nil).getOrElse(Nil)
    ))
  }
}
class WordDelimiterTokenFilter(override val filterName: String,
                               catenate_all: Option[Boolean] = None, //false
                               catenate_numbers: Option[Boolean] = None, //false
                               catenate_words: Option[Boolean] = None, //false
                               generate_number_parts: Option[Boolean] = None, //false
                               generate_word_parts: Option[Boolean] = None, //false
                               preserve_original: Option[Boolean] = None, //false
                               protected_words: Seq[String] = Nil,
                               protected_words_path: Option[String] = None,
                               split_on_case_change: Option[Boolean] = None, //true
                               split_on_numerics: Option[String] = None, //true
                               stem_english_possessive: Option[String] = None, //true
                               type_table: Option[String] = None,
                               type_table_path: Option[String] = None
                              ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.word_delimiter

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      catenate_all.map(sp => JField("catenate_all", JBool(sp)) :: Nil).getOrElse(Nil)
    ))
  }
}
class WordDelimiterGraphTokenFilter(override val filterName: String,
                                    adjust_offsets: Option[Boolean] = None, //true
                                    catenate_all: Option[Boolean] = None, //false
                                    catenate_numbers: Option[Boolean] = None, //false
                                    catenate_words: Option[Boolean] = None, //false
                                    generate_number_parts: Option[Boolean] = None, //false
                                    generate_word_parts: Option[Boolean] = None, //false
                                    preserve_original: Option[Boolean] = None, //false
                                    protected_words: Seq[String] = Nil,
                                    protected_words_path: Option[String] = None,
                                    split_on_case_change: Option[Boolean] = None, //true
                                    split_on_numerics: Option[String] = None, //true
                                    stem_english_possessive: Option[String] = None, //true
                                    type_table: Option[String] = None,
                                    type_table_path: Option[String] = None
                                   ) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.word_delimiter_graph

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      catenate_all.map(sp => JField("catenate_all", JBool(sp)) :: Nil).getOrElse(Nil)
    ))
  }
}

class ICUTransformTokenFilter(override val filterName: String,
                              id: String) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.icu_transform

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("id", JString(id)) :: Nil
    ))
  }
}

object PhoneticEncoder extends Enumeration {
  val double_metaphone, soundex, refined_soundex, caverphone1, caverphone2,
  cologne, nysiis, koelnerphonetik, haasephonetik, beider_morse, daitch_mokotoff = Value
}
class PhoneticTokenFilter(override val filterName: String,
                          encoder: PhoneticEncoder.Value) extends AbstractTokenFilter(filterName) {
  override val filterType: TokenFilters.Value = TokenFilters.phonetic

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("encoder", JString(encoder.toString)) :: Nil
    ))
  }
}