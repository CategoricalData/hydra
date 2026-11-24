package hydra.core.overlay.scala.lib

object literals:
  // Haskell uses round (half-even / banker's rounding)
  def bigintToDecimal(x: BigInt): BigDecimal = BigDecimal(x)
  def bigintToInt(x: BigInt)(it: Any): Any = it // Placeholder
  def bigintToInt8(x: BigInt): Byte = x.toByte
  def bigintToInt16(x: BigInt): Short = x.toShort
  def bigintToInt32(x: BigInt): Int = x.toInt
  def bigintToInt64(x: BigInt): Long = x.toLong
  def bigintToIntegerValue(x: BigInt): Any = x // Placeholder
  def bigintToUint(x: BigInt)(it: Any): Any = it // Placeholder
  def bigintToUint8(x: BigInt): Byte = (x & 0xff).toByte
  def bigintToUint16(x: BigInt): Int = (x & 0xffff).toInt
  def bigintToUint32(x: BigInt): Long = (x & 0xffffffffL).toLong
  def bigintToUint64(x: BigInt): BigInt = x & BigInt("ffffffffffffffff", 16)
  def binaryToBytes(b: String): Seq[Int] =
    try java.util.Base64.getDecoder.decode(b).toSeq.map(_.toInt & 0xff)
    catch case _: IllegalArgumentException => b.getBytes("ISO-8859-1").toSeq.map(_.toInt & 0xff)
  def binaryToBase64(b: String): String = b
  def decimalToBigint(x: BigDecimal): BigInt = x.setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt
  def decimalToFloat32(x: BigDecimal): Float = x.toFloat
  def decimalToFloat64(x: BigDecimal): Double = x.toDouble
  def float(ft: Any)(x: BigDecimal): Any = x // Placeholder
  // BigDecimal cannot represent NaN or Infinity; use sentinel zero (mirrors Java).
  // Uses shortestDigitsFloat32/64 rather than Float/Double.toString, which does not always
  // produce the shortest round-tripping decimal for subnormal values on JDK versions prior to 19
  // (JDK-4511638; see shortestDigitsFloat64 and #763).
  def float32ToDecimal(x: Float): BigDecimal =
    if x.isNaN || x.isInfinite then BigDecimal(0)
    else if x == 0.0f then BigDecimal(0)
    else
      val d = shortestDigitsFloat32(x)
      val unscaled = new java.math.BigDecimal(new java.math.BigInteger(d.digits))
        .scaleByPowerOfTen(d.exponent - (d.digits.length - 1))
      val result = BigDecimal(unscaled)
      if d.negative then -result else result
  def float32ToFloat64(x: Float): Double = x.toDouble
  def float64ToDecimal(x: Double): BigDecimal =
    if x.isNaN || x.isInfinite then BigDecimal(0)
    else if x == 0.0 then BigDecimal(0)
    else
      val d = shortestDigitsFloat64(x)
      val unscaled = new java.math.BigDecimal(new java.math.BigInteger(d.digits))
        .scaleByPowerOfTen(d.exponent - (d.digits.length - 1))
      val result = BigDecimal(unscaled)
      if d.negative then -result else result
  def float64ToFloat32(x: Double): Float = x.toFloat
  def int(it: Any)(x: BigInt): Any = x // Placeholder
  def int8ToBigint(x: Byte): BigInt = BigInt(x)
  def int16ToBigint(x: Short): BigInt = BigInt(x)
  def int32ToBigint(x: Int): BigInt = BigInt(x)
  def int32ToBigint(x: Byte): BigInt = BigInt(x.toInt)
  def int64ToBigint(x: Long): BigInt = BigInt(x)
  def integerValueToBigint(x: Any): BigInt = x match
    case i: Int => BigInt(i)
    case l: Long => BigInt(l)
    case b: BigInt => b
    case _ => BigInt(0)
  def parseBigint(s: String): Option[BigInt] = try Some(BigInt(s)) catch { case _: Exception => None }
  def parseBoolean(s: String): Option[Boolean] = s.toBooleanOption
  def parseDecimal(s: String): Option[BigDecimal] = try Some(BigDecimal(s)) catch { case _: Exception => None }
  def readFloat(s: String)(ft: Any): Option[Any] = try Some(BigDecimal(s)) catch { case _: Exception => None }
  def parseFloat32(s: String): Option[Float] = s.toFloatOption
  def parseFloat64(s: String): Option[Double] = s.toDoubleOption
  def readInt(s: String)(it: Any): Option[Any] = s.toIntOption
  def parseInt8(s: String): Option[Byte] = s.toByteOption
  def parseInt16(s: String): Option[Short] = s.toShortOption
  def parseInt32(s: String): Option[Int] = s.toIntOption
  def parseInt64(s: String): Option[Long] = s.toLongOption
  def parseString(s: String): Option[String] =
    // Haskell read expects a quoted string: read "\"hello\"" = "hello"
    if s.startsWith("\"") && s.endsWith("\"") && s.length >= 2 then
      Some(s.substring(1, s.length - 1)
        .replace("\\\\", "\u0000").replace("\\\"", "\"")
        .replace("\\n", "\n").replace("\\t", "\t").replace("\\r", "\r")
        .replace("\\a", "\u0007").replace("\\b", "\b").replace("\\f", "\f")
        .replace("\\v", "\u000b")
        .replace("\u0000", "\\"))
    else None
  def readUint(s: String)(it: Any): Option[Any] = s.toIntOption.filter(_ >= 0)
  def parseUint8(s: String): Option[Byte] = s.toShortOption.filter(n => n >= 0 && n <= 255).map(_.toByte)
  def parseUint16(s: String): Option[Int] = s.toIntOption.filter(n => n >= 0 && n <= 65535)
  def parseUint32(s: String): Option[Long] = s.toLongOption.filter(n => n >= 0 && n <= 4294967295L)
  def parseUint64(s: String): Option[BigInt] = try { val n = BigInt(s); if (n >= 0 && n <= BigInt("18446744073709551615")) Some(n) else None } catch { case _: Exception => None }

  // Significant digits (trailing zeros stripped) and base-10 exponent of the leading digit.
  private case class Digits(negative: Boolean, digits: String, exponent: Int):
    def toScientific: String =
      val mantissa = if digits.length == 1 then digits + ".0" else digits.charAt(0) + "." + digits.substring(1)
      (if negative then "-" else "") + mantissa + "e" + exponent
    def toDecimal: String =
      val sign = if negative then "-" else ""
      if exponent >= 0 then
        val intPart =
          if digits.length > exponent + 1 then digits.substring(0, exponent + 1)
          else digits + "0" * (exponent + 1 - digits.length)
        val fracPart = if digits.length > exponent + 1 then digits.substring(exponent + 1) else "0"
        sign + intPart + "." + (if fracPart.isEmpty then "0" else fracPart)
      else
        sign + "0." + "0" * (-exponent - 1) + digits

  // Decomposes a "%e"-formatted string (e.g. "5.000000000000000e-324") into digits + exponent.
  private def decompose(formatted: String, negative: Boolean): Digits =
    val eIdx = formatted.indexOf('e')
    val mantissa = formatted.substring(0, eIdx).replace(".", "")
    val exponent = formatted.substring(eIdx + 1).toInt
    var end = mantissa.length
    while end > 1 && mantissa.charAt(end - 1) == '0' do end -= 1
    Digits(negative, mantissa.substring(0, end), exponent)

  // Finds the shortest decimal digit string that round-trips to the exact double bit pattern.
  // Does NOT delegate to Double.toString for digit selection: it does not always produce the
  // shortest round-tripping decimal for subnormal values on JDK versions prior to 19
  // (JDK-4511638; e.g. Double.toString(Double.MIN_VALUE) gives "4.9E-324" instead of the correct,
  // shorter "5E-324"). Instead this searches increasing significant-digit counts and verifies
  // each candidate round-trips back to the exact input via parseDouble (#763).
  private def shortestDigitsFloat64(value: Double): Digits =
    val negative = value < 0
    val abs = scala.math.abs(value)
    (1 to 17).iterator
      .map(sig => String.format(java.util.Locale.ROOT, s"%.${sig - 1}e", abs))
      .find(s => s.toDouble == abs)
      .map(decompose(_, negative))
      .getOrElse(decompose(String.format(java.util.Locale.ROOT, "%.16e", abs), negative))

  // Finds the shortest decimal digit string that round-trips to the exact float bit pattern.
  private def shortestDigitsFloat32(value: Float): Digits =
    val negative = value < 0
    val abs = scala.math.abs(value)
    (1 to 9).iterator
      .map(sig => String.format(java.util.Locale.ROOT, s"%.${sig - 1}e", abs))
      .find(s => s.toFloat == abs)
      .map(decompose(_, negative))
      .getOrElse(decompose(String.format(java.util.Locale.ROOT, "%.8e", abs), negative))

  // Format a float32 to match Hydra's show for Float (Haskell-compatible)
  private def showHaskellFloat32(x: Float): String =
    if x.isNaN then "NaN"
    else if x.isInfinite then if x > 0 then "Infinity" else "-Infinity"
    else if x == 0.0f then if (java.lang.Float.floatToRawIntBits(x) != 0) "-0.0" else "0.0"
    else
      val d = shortestDigitsFloat32(x)
      val abs = scala.math.abs(x)
      if abs >= 0.1f && abs < 1.0e7f then d.toDecimal else d.toScientific

  // Format a double to match Hydra's show for Double (Haskell-compatible)
  private def showHaskellDouble(x: Double): String =
    if x.isNaN then "NaN"
    else if x.isInfinite then if x > 0 then "Infinity" else "-Infinity"
    else if x == 0.0 then if (java.lang.Double.doubleToRawLongBits(x) != 0) "-0.0" else "0.0"
    else
      val d = shortestDigitsFloat64(x)
      val abs = scala.math.abs(x)
      if abs >= 0.1 && abs < 1.0e7 then d.toDecimal else d.toScientific

  def printBigint(x: BigInt): String = x.toString
  def printBoolean(x: Boolean): String = if x then "true" else "false"
  // Representation-faithful decimal printer, per docs/specification/json-format.md
  // (Decimal formatting) and syntax.md (2.6): coefficient digits -- trailing
  // zeros included -- are preserved exactly, and zero prints per its scale
  // ("0", "0.0", "0.00"). Layout follows ECMAScript Number::toString / RFC 8785:
  // positional form when the adjusted exponent a satisfies -6 <= a < 21,
  // exponent form otherwise (one digit before the point, lowercase e,
  // always-signed exponent, coefficient digits preserved).
  def printDecimal(x: BigDecimal): String = {
    val scale = x.scale
    val digits = x.underlying.unscaledValue.abs.toString
    val n = digits.length
    val sign = if (x.signum < 0) "-" else ""
    val a = n - 1 - scale
    if (a >= -6 && a < 21) {
      sign + printDecimalPositional(digits, scale, n)
    } else {
      val mantissa = if (n == 1) digits + ".0" else digits.charAt(0).toString + "." + digits.substring(1)
      s"$sign${mantissa}e${if (a >= 0) "+" else ""}$a"
    }
  }

  // Places the decimal point `scale` digits from the right, padding with
  // zeros on either side as needed.
  private def printDecimalPositional(digits: String, scale: Int, n: Int): String =
    if (scale <= 0) digits + "0" * (-scale)
    else if (scale < n) digits.substring(0, n - scale) + "." + digits.substring(n - scale)
    else "0." + "0" * (scale - n) + digits
  def showFloat(x: Any)(ft: Any): String = x.toString
  def printFloat32(x: Float): String = showHaskellFloat32(x)
  def printFloat64(x: Double): String = showHaskellDouble(x)
  def showInt(x: Any)(it: Any): String = x.toString
  def printInt8(x: Byte): String = x.toString
  def printInt16(x: Short): String = x.toString
  def printInt32(x: Int): String = x.toString
  def printInt64(x: Long): String = x.toString

  // Haskell show for strings: escapes special chars and non-ASCII
  def printString(s: String): String =
    val sb = new StringBuilder("\"")
    s.codePoints().forEach { cp =>
      cp match
        case '\\' => sb.append("\\\\")
        case '"' => sb.append("\\\"")
        case '\u0007' => sb.append("\\a")   // bell
        case '\b' => sb.append("\\b")       // backspace
        case '\t' => sb.append("\\t")       // tab
        case '\n' => sb.append("\\n")       // newline
        case '\u000b' => sb.append("\\v")   // vertical tab
        case '\f' => sb.append("\\f")       // form feed
        case '\r' => sb.append("\\r")       // carriage return
        case 0x7f => sb.append("\\DEL")     // delete
        case 0 => sb.append("\\NUL")        // null
        case c if c >= 32 && c < 127 => sb.append(c.toChar)
        case c => sb.append("\\").append(c) // Non-ASCII: numeric escape
    }
    sb.append("\"").toString

  def showUint(x: Any)(it: Any): String = x.toString
  def printUint8(x: Byte): String = (x.toInt & 0xff).toString
  def printUint16(x: Int): String = x.toString
  def printUint32(x: Long): String = x.toString
  def printUint64(x: BigInt): String = x.toString
  def base64ToBinary(s: String): String = s
  def uint(it: Any)(x: BigInt): Any = x // Placeholder
  def uint8ToBigint(x: Byte): BigInt = BigInt(x.toInt & 0xff)
  def uint16ToBigint(x: Int): BigInt = BigInt(x)
  def uint32ToBigint(x: Long): BigInt = BigInt(x)
  def uint64ToBigint(x: BigInt): BigInt = x
