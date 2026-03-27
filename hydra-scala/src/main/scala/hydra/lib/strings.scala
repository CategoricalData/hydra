package hydra.lib

object strings:
  /** Concatenate a list of strings into a single string. */
  def cat(ss: Seq[String]): String = ss.mkString

  /** Concatenate two strings. */
  def cat2(a: String)(b: String): String = a + b

  /** Get the Unicode code point of the character at a specific index in a string. */
  def charAt(i: Int)(s: String): Int = if i >= 0 && i < s.length then s.codePointAt(i) else 0

  /** Convert a list of Unicode code points to a string. */
  def fromList(cs: Seq[Int]): String = new String(cs.flatMap(Character.toChars(_)).toArray)

  /** Join a list of strings with a separator between each element. */
  def intercalate(sep: String)(ss: Seq[String]): String = ss.mkString(sep)

  /** Check whether a string is empty. */
  def isEmpty(s: String): Boolean = s.isEmpty

  /** Return the length of a string. */
  def length(s: String): Int = s.codePointCount(0, s.length)

  /** Split a string into lines. */
  def lines(s: String): Seq[String] =
    if s.isEmpty then Seq.empty
    else
      val parts = s.split("\n", -1).toSeq
      // Haskell: lines "a\nb\n" = ["a","b"], lines "a\nb" = ["a","b"]
      if parts.last.isEmpty then parts.init else parts

  /** Check whether a string is empty. */
  def `null`(s: String): Boolean = s.isEmpty

  /** Split a string on a delimiter string. */
  def splitOn(sep: String)(s: String): Seq[String] =
    if sep.isEmpty then
      // Haskell Data.List.Split.splitOn "" "abc" = ["","a","b","c"]
      "" +: s.map(_.toString).toSeq
    else s.split(java.util.regex.Pattern.quote(sep), -1).toSeq

  /** Convert a string to a list of Unicode code points. */
  def toList(s: String): Seq[Int] = s.codePoints().toArray.toSeq

  /** Convert a string to lowercase. */
  def toLower(s: String): String = s.toLowerCase

  /** Convert a string to uppercase. */
  def toUpper(s: String): String = s.toUpperCase

  /** Join a list of strings with newlines, appending a trailing newline. */
  // Haskell: unlines ["a","b"] = "a\nb\n" (appends newline to each)
  def unlines(ss: Seq[String]): String = ss.map(_ + "\n").mkString
