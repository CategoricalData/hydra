package hydra.lib.strings


def cat(lists: Seq[String]): String = lists.mkString

def length(s: String): Int = s.length

def splitOn(sep: String, s: String): Seq[String] = {
  // The "cap" enforces behavior consistent with Data.List.Split in Haskell
  var cap = if (sep.startsWith("-")) "_" else "-"
  (s + sep + cap).split(sep).dropRight(1)
}

def toLower(s: String): String = s.toLowerCase()

def toUpper(s: String): String = s.toUpperCase()
