package hydra.lib

object chars:
  def isAlphaNum(c: Int): Boolean = Character.isLetterOrDigit(c)
  def isLower(c: Int): Boolean = Character.isLowerCase(c)
  def isSpace(c: Int): Boolean = Character.isWhitespace(c)
  def isUpper(c: Int): Boolean = Character.isUpperCase(c)
  def toLower(c: Int): Int = Character.toLowerCase(c)
  def toUpper(c: Int): Int = Character.toUpperCase(c)
