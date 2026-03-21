package hydra.lib

object strings:
  def cat(ss: Seq[String]): String = ss.mkString
  def cat2(a: String)(b: String): String = a + b
  def charAt(i: Int)(s: String): Int = if i >= 0 && i < s.length then s.codePointAt(i) else 0
  def fromList(cs: Seq[Int]): String = new String(cs.map(_.toChar).toArray)
  def intercalate(sep: String)(ss: Seq[String]): String = ss.mkString(sep)
  def isEmpty(s: String): Boolean = s.isEmpty
  def length(s: String): Int = s.length
  def lines(s: String): Seq[String] = s.split("\n", -1).toSeq
  def `null`(s: String): Boolean = s.isEmpty
  def splitOn(sep: String)(s: String): Seq[String] =
    if sep.isEmpty then s.map(_.toString).toSeq
    else s.split(java.util.regex.Pattern.quote(sep), -1).toSeq
  def toList(s: String): Seq[Int] = s.codePoints().toArray.toSeq
  def toLower(s: String): String = s.toLowerCase
  def toUpper(s: String): String = s.toUpperCase
  def unlines(ss: Seq[String]): String = ss.mkString("\n")
