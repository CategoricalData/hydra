package hydra.lib

object strings:
  def cat(ss: Seq[String]): String = ss.mkString
  def cat2(a: String)(b: String): String = a + b
  def charAt(i: Int)(s: String): Int = if i >= 0 && i < s.length then s.codePointAt(i) else 0
  def fromList(cs: Seq[Int]): String = new String(cs.flatMap(Character.toChars(_)).toArray)
  def intercalate(sep: String)(ss: Seq[String]): String = ss.mkString(sep)
  def isEmpty(s: String): Boolean = s.isEmpty
  def length(s: String): Int = s.codePointCount(0, s.length)
  def lines(s: String): Seq[String] =
    if s.isEmpty then Seq.empty
    else
      val parts = s.split("\n", -1).toSeq
      // Haskell: lines "a\nb\n" = ["a","b"], lines "a\nb" = ["a","b"]
      if parts.last.isEmpty then parts.init else parts
  def `null`(s: String): Boolean = s.isEmpty
  def splitOn(sep: String)(s: String): Seq[String] =
    if sep.isEmpty then
      // Haskell Data.List.Split.splitOn "" "abc" = ["","a","b","c"]
      "" +: s.map(_.toString).toSeq
    else s.split(java.util.regex.Pattern.quote(sep), -1).toSeq
  def toList(s: String): Seq[Int] = s.codePoints().toArray.toSeq
  def toLower(s: String): String = s.toLowerCase
  def toUpper(s: String): String = s.toUpperCase
  // Haskell: unlines ["a","b"] = "a\nb\n" (appends newline to each)
  def unlines(ss: Seq[String]): String = ss.map(_ + "\n").mkString
