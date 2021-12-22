package hydra.lib.strings


def cat(lists: Seq[String]): String = lists.mkString

def length(s: String): Int = s.length

def splitOn(sep: String, s: String): Seq[String] = s.split(sep)

def toLower(s: String): String = s.toLowerCase()

def toUpper(s: String): String = s.toUpperCase()
