package hydra.lib

import java.util.regex.{Matcher, Pattern}

object regex:
  /** Find the first substring matching a regex pattern. */
  def find(pattern: String)(input: String): Option[String] =
    val m = Pattern.compile(pattern).matcher(input)
    if m.find() then Some(m.group()) else None

  /** Find all non-overlapping substrings matching a regex pattern. */
  def findAll(pattern: String)(input: String): Seq[String] =
    val m = Pattern.compile(pattern).matcher(input)
    val results = scala.collection.mutable.ArrayBuffer[String]()
    while m.find() do results += m.group()
    results.toSeq

  /** Check whether an entire string matches a regex pattern. */
  def matches(pattern: String)(input: String): Boolean =
    Pattern.matches(pattern, input)

  /** Replace the first occurrence of a regex pattern with a replacement string. */
  def replace(pattern: String)(replacement: String)(input: String): String =
    Pattern.compile(pattern).matcher(input).replaceFirst(Matcher.quoteReplacement(replacement))

  /** Replace all non-overlapping occurrences of a regex pattern with a replacement string. */
  def replaceAll(pattern: String)(replacement: String)(input: String): String =
    Pattern.compile(pattern).matcher(input).replaceAll(Matcher.quoteReplacement(replacement))

  /** Split a string by a regex pattern. */
  def split(pattern: String)(input: String): Seq[String] =
    Pattern.compile(pattern).split(input, -1).toSeq
