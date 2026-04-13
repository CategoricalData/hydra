package hydra.lib

import _root_.java.util.regex.{Matcher, Pattern}

object regex:
  def matches(pattern: String)(input: String): Boolean =
    Pattern.matches(pattern, input)

  def find(pattern: String)(input: String): Option[String] =
    val m = Pattern.compile(pattern).matcher(input)
    if m.find() then Some(m.group()) else None

  def findAll(pattern: String)(input: String): Seq[String] =
    val m = Pattern.compile(pattern).matcher(input)
    val results = _root_.scala.collection.mutable.ArrayBuffer[String]()
    while m.find() do results += m.group()
    results.toSeq

  def replace(pattern: String)(replacement: String)(input: String): String =
    Pattern.compile(pattern).matcher(input).replaceFirst(Matcher.quoteReplacement(replacement))

  def replaceAll(pattern: String)(replacement: String)(input: String): String =
    Pattern.compile(pattern).matcher(input).replaceAll(Matcher.quoteReplacement(replacement))

  def split(pattern: String)(input: String): Seq[String] =
    Pattern.compile(pattern).split(input, -1).toSeq
