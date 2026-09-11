package hydra.overlay.scala.lib

object strings:
  def concat(ss: Seq[String]): String = ss.mkString
  def concat2(a: String)(b: String): String = a + b
  def fromList(cs: Seq[Int]): String = new String(cs.flatMap(Character.toChars(_)).toArray)
  def join(sep: String)(ss: Seq[String]): String = ss.mkString(sep)
  def isEmpty(s: String): Boolean = s.isEmpty
  def length(s: String): Int = s.codePointCount(0, s.length)
  // i is a code-point index, not a UTF-16 code-unit offset: bound via codePointCount and
  // translate to a UTF-16 offset via offsetByCodePoints before indexing, mirroring the Java
  // host's overlay/java/.../lib/strings/CharAt.java (astral chars occupy two UTF-16 units).
  def charAt(i: Int)(s: String): Option[Int] =
    val len = s.codePointCount(0, s.length)
    if i >= 0 && i < len then Some(s.codePointAt(s.offsetByCodePoints(0, i))) else None
  def splitOn(sep: String)(s: String): Seq[String] =
    if sep.isEmpty then
      // Haskell Data.List.Split.splitOn "" "abc" = ["","a","b","c"]
      "" +: s.map(_.toString).toSeq
    else s.split(java.util.regex.Pattern.quote(sep), -1).toSeq
  def toList(s: String): Seq[Int] = s.codePoints().toArray.toSeq
  def toLower(s: String): String = s.toLowerCase
  def toUpper(s: String): String = s.toUpperCase
