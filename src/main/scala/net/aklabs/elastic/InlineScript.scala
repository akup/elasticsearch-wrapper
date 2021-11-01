package net.aklabs.elastic

import net.aklabs.helpers.JsonHelpers._

class ScriptObject(val lang: Option[String] = None, //painless
                   val source: Option[String] = None,
                   val id: Option[String] = None,
                   val params: Map[String, Any] = Map.empty) {
  def toJsonField: JField = {
    if (source.isEmpty && id.isEmpty)
      throw new Exception("source or id should be defined")
    JField("script", JObject(
      source.map(s => JField("source", JString(s)))
        .getOrElse(JField("id", JString(id.get))) ::
        lang.map(l => JField("lang", JString(l)) :: Nil).getOrElse(Nil) :::
        ( if (params.isEmpty) Nil else JField("params", any2JValue(params, "params")) :: Nil )
    ))
  }
}
class InlineScript(the_source: String,
                   override val lang: Option[String] = None,
                   override val params: Map[String, Any] = Map.empty
                  ) extends ScriptObject(source = Some(the_source))
class InlinePainlessScript(the_source: String,
                   override val params: Map[String, Any] = Map.empty
                  ) extends ScriptObject(lang = Some("painless"), source = Some(the_source))
