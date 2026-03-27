package hydra.lib

object chars:
  /** Check whether a character is alphanumeric. */
  def isAlphaNum(c: Int): Boolean = Character.isLetterOrDigit(c)

  /** Check whether a character is lowercase. */
  def isLower(c: Int): Boolean = Character.isLowerCase(c)

  /** Check whether a character is a whitespace character. */
  def isSpace(c: Int): Boolean = Character.isWhitespace(c)

  /** Check whether a character is uppercase. */
  def isUpper(c: Int): Boolean = Character.isUpperCase(c)

  /** Convert a character to lowercase. */
  def toLower(c: Int): Int = Character.toLowerCase(c)

  /** Convert a character to uppercase. */
  def toUpper(c: Int): Int = Character.toUpperCase(c)
