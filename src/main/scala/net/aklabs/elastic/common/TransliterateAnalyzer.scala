package net.aklabs.elastic.common

object TransliterateAnalyzer {
  private val supportedLanguages = Set("ru", "en")

  def name(language: String) = {
    if (supportedLanguages.contains(language))
      Some("transliterator_%s".format(language))
    else None
  }

  def get(language: String) = {
    language.toLowerCase match {
      case "ru" =>
        Some()
      case _ => None
    }
  }
}
