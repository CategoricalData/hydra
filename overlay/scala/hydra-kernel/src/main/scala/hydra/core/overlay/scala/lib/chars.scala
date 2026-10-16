package hydra.core.overlay.scala.lib

object chars:
  def isAlpha(c: Int): Boolean = Character.isLetter(c)
  def isAlphaNum(c: Int): Boolean = Character.isLetterOrDigit(c)
  // ASCII-only, matching Haskell's Data.Char.isDigit -- Character.isDigit(c) is Unicode-aware
  // and would wrongly accept non-ASCII digits (e.g. Arabic-Indic digits).
  def isDigit(c: Int): Boolean = c >= '0' && c <= '9'
  def isLower(c: Int): Boolean = Character.isLowerCase(c)
  def isSpace(c: Int): Boolean = Character.isWhitespace(c)
  def isUpper(c: Int): Boolean = Character.isUpperCase(c)
  def toLower(c: Int): Int = Character.toLowerCase(c)
  def toUpper(c: Int): Int = Character.toUpperCase(c)
