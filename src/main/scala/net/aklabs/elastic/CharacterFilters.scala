package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._

object CharacterFilters extends Enumeration {
  val html_strip, mapping, pattern_replace = Value
}
abstract class AbstractCharacterFilter(val filterName: String) {
  val filterType: CharacterFilters.Value

  def toJsonField: JField
}
class HtmlStripCharacterFilter(override val filterName: String,
                               escaped_tags: Seq[String] = Nil
                              ) extends AbstractCharacterFilter(filterName) {
  override val filterType: CharacterFilters.Value = CharacterFilters.html_strip

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      ( if (escaped_tags.isEmpty) Nil else
      JField("escaped_tags", JArray(escaped_tags.map(JString(_)))) :: Nil )
    ))
  }
}
class MappingCharacterFilter(override val filterName: String,
                             mappings: Seq[String] = Nil,
                             mappings_path: Option[String]
                            ) extends AbstractCharacterFilter(filterName) {
  override val filterType: CharacterFilters.Value = CharacterFilters.mapping

  override def toJsonField: JField = {
    if (mappings.isEmpty && mappings_path.isEmpty)
      throw new Exception("mappings or mappings_path should not be empty")
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      ( if (mappings.isEmpty) JField("mappings_path", JString(mappings_path.get)) :: Nil else
        JField("mappings", JArray(mappings.map(JString(_)))) :: Nil )
    ))
  }
}
class PatternReplaceCharacterFilter(override val filterName: String,
                                pattern: String,
                                replacement: Option[String] = None, //""
                                all: Option[Boolean] = None //true
                               ) extends AbstractCharacterFilter(filterName) {
  override val filterType: CharacterFilters.Value = CharacterFilters.pattern_replace

  override def toJsonField: JField = {
    JField(filterName, JObject(JField("type", JString(filterType.toString)) ::
      JField("pattern", JString(pattern)) ::
      replacement.map(r => JField("replacement", JString(r)) :: Nil).getOrElse(Nil) :::
      all.map(a => JField("all", JBool(a)) :: Nil).getOrElse(Nil) :::
      Nil
    ))
  }
}
