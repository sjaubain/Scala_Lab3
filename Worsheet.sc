

import Anagrams.{FingerPrint, subseqs, subtract}

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

def subseqs(fp: String): List[String] = {

  // On utilise un Set pour ne pas introduire une valeur deja existante
  def subseqsWithoutDuplicate(fp: String): Set[String] = {
    if (fp.length == 0)
      Set("")
    else {
      for {
        firstChar <- Set("", fp(0)) // on prend soit le premier caractère soit la chaine vide
        remaining <- subseqs(fp.substring(1))
      } yield firstChar + remaining // puis on concatène recursivement avec la chaine otée du premier caractère
    }
  }

  subseqsWithoutDuplicate(fp).toList
}

def test() = {
  for {
    i <- 1 until 10
    j <- 1 until i
    if i + j == 7
  } yield (i, j)
}

/*
"abbc" take 2
subseqs("abbc")
val test = "test"
val test 2 = test(1) :: "d" :: Nil
for {
  s <- test2
} yield s
*/

def subtract(x: String, y: String): String = (x, y) match {
  case ("", str) => ""
  case (str, "") => str
  case (a, b) => {
    if (a(0) == b(0)) subtract(a drop 1, b drop 1)
    else a(0) + subtract(a drop 1, b)
  }
}

subtract("sdfrbdf", "dfd")