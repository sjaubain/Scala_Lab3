

import scala.collection.immutable.{List, Map}


def fingerPrint1(s: String): String = s.toLowerCase.sorted

fingerPrint1("Robel")

def fingerPrint(s: List[String]): String = fingerPrint1(s.flatten.mkString)

fingerPrint("Robel et Simon".split(" ").toList)

val dictionary: List[String] =
  List("ate", "eat", "tea", "pot", "top", "sonja", "jason", "normal",
    "I", "love", "you", "olive")
val matchingWords: Map[String, List[String]] = dictionary.groupBy(fingerPrint1)
  //

