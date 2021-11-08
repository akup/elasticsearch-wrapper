# elasticsearch-wrapper

Обёртка вокруг курла для вызова elasticsearch и набор классов для представления ответов и запросов для Scala.

Зависит от модуля global_props [Mindweb http server](https://github.com/akup/mindweb-http-server).
На самом деле зависимость только от JsonHelper - code sugar для Jckson, который нужно вынести в отдельный проект.
Позволяет писать вот такой вот код, который имплиситами будет приведён к json нодам, без необходимости прогонять jckson mapper на java/scala типах.
```scala
  val optional: Option[Boolean] = Some(true)
  val o: JObject = JObject(
    "f1" -> "f1",
    "f2" -> 0,
    "arr" -> Seq(1, 2, 3, 4, Seq(1, 2, 3), JObject(
      "inner", 0.6
    )),
    optional.map("opt" -> _),
    None
  )

  val f2: BigInt = o \ "f2" match {
    case JInt(i) => i.toInt
  }
  val flattenedArray: Iterable[Double] = (o \ "arr" match {
    case JArray(arr) => arr.collect {
      case x if (x \ "inner" match {
        case JDouble(_) => true;
        case _ => false
      }) => Seq((x \ "inner").doubleValue())
      case JInt(i) => Seq(i.doubleValue())
      case JArray(innerArr) => innerArr.map(_.doubleValue())
    }
  }).flatten
```
