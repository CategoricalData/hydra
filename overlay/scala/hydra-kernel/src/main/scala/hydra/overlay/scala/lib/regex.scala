package hydra.overlay.scala.lib

import _root_.java.util.regex.{Matcher, Pattern}

// Patterns are Hydra-defined and translingual (docs/specification/regex.md). Each primitive first
// runs the pattern through hydra.parse.regex, then renders the AST to PCRE syntax via
// hydra.print.pcre.regex (Scala uses java.util.regex, a PCRE-like engine), before handing the
// rendered pattern to the native engine. An ill-formed pattern (rejected by hydra.parse.regex) is
// treated as "no match" — the same portable-failure convention as an empty match. See issue #603.
object regex:
  private def toNative(pattern: String): Option[String] =
    hydra.parse.regex.parseRegex(pattern).map(hydra.print.pcre.regex.printRegex)

  def matches(pattern: String)(input: String): Boolean =
    toNative(pattern) match
      case None => false
      case Some(native) => Pattern.matches(native, input)

  def find(pattern: String)(input: String): Option[String] =
    toNative(pattern) match
      case None => None
      case Some(native) =>
        val m = Pattern.compile(native).matcher(input)
        if m.find() then Some(m.group()) else None

  def findAll(pattern: String)(input: String): Seq[String] =
    toNative(pattern) match
      case None => Seq()
      case Some(native) =>
        val m = Pattern.compile(native).matcher(input)
        val results = _root_.scala.collection.mutable.ArrayBuffer[String]()
        while m.find() do results += m.group()
        results.toSeq

  def replace(pattern: String)(replacement: String)(input: String): String =
    toNative(pattern) match
      case None => input
      case Some(native) =>
        Pattern.compile(native).matcher(input).replaceFirst(Matcher.quoteReplacement(replacement))

  def replaceAll(pattern: String)(replacement: String)(input: String): String =
    toNative(pattern) match
      case None => input
      case Some(native) =>
        Pattern.compile(native).matcher(input).replaceAll(Matcher.quoteReplacement(replacement))

  def split(pattern: String)(input: String): Seq[String] =
    toNative(pattern) match
      case None => Seq(input)
      case Some(native) => Pattern.compile(native).split(input, -1).toSeq
