package hydra.lib

object literals:
  /** Convert a bigfloat to a bigint. */
  // Haskell uses round (half-even / banker's rounding)
  def bigfloatToBigint(x: BigDecimal): BigInt = x.setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt

  /** Convert a bigfloat to a float value (placeholder). */
  def bigfloatToFloat(x: BigDecimal)(ft: Any): Any = ft // Placeholder

  /** Convert a bigfloat to a float32. */
  def bigfloatToFloat32(x: BigDecimal): Float = x.toFloat

  /** Convert a bigfloat to a float64. */
  def bigfloatToFloat64(x: BigDecimal): Double = x.toDouble

  /** Convert a bigfloat to a float value (placeholder). */
  def bigfloatToFloatValue(x: BigDecimal): Any = x // Placeholder

  /** Convert a bigint to a bigfloat. */
  def bigintToBigfloat(x: BigInt): BigDecimal = BigDecimal(x)

  /** Convert a bigint to an integer value (placeholder). */
  def bigintToInt(x: BigInt)(it: Any): Any = it // Placeholder

  /** Convert a bigint to an int16. */
  def bigintToInt16(x: BigInt): Short = x.toShort

  /** Convert a bigint to an int32. */
  def bigintToInt32(x: BigInt): Int = x.toInt

  /** Convert a bigint to an int64. */
  def bigintToInt64(x: BigInt): Long = x.toLong

  /** Convert a bigint to an int8. */
  def bigintToInt8(x: BigInt): Byte = x.toByte

  /** Convert a bigint to an integer value (placeholder). */
  def bigintToIntegerValue(x: BigInt): Any = x // Placeholder

  /** Convert a bigint to an unsigned integer value (placeholder). */
  def bigintToUint(x: BigInt)(it: Any): Any = it // Placeholder

  /** Convert a bigint to a uint16. */
  def bigintToUint16(x: BigInt): Int = (x & 0xffff).toInt

  /** Convert a bigint to a uint32. */
  def bigintToUint32(x: BigInt): Long = (x & 0xffffffffL).toLong

  /** Convert a bigint to a uint64. */
  def bigintToUint64(x: BigInt): BigInt = x & BigInt("ffffffffffffffff", 16)

  /** Convert a bigint to a uint8. */
  def bigintToUint8(x: BigInt): Byte = (x & 0xff).toByte

  /** Convert binary to a list of byte values (0-255). */
  def binaryToBytes(b: String): Seq[Int] =
    try java.util.Base64.getDecoder.decode(b).toSeq.map(_.toInt & 0xff)
    catch case _: IllegalArgumentException => b.getBytes("ISO-8859-1").toSeq.map(_.toInt & 0xff)

  /** Convert binary to string by base64 encoding. */
  def binaryToString(b: String): String = b

  /** Convert a float value to a typed float (placeholder). */
  def float(ft: Any)(x: BigDecimal): Any = x // Placeholder

  /** Convert a float32 to a bigfloat. */
  def float32ToBigfloat(x: Float): BigDecimal = BigDecimal(x.toDouble)

  /** Convert a float64 to a bigfloat. */
  def float64ToBigfloat(x: Double): BigDecimal = BigDecimal(x)

  /** Convert a float value to a bigfloat. */
  def floatValueToBigfloat(x: Any): BigDecimal = x match
    case d: BigDecimal => d
    case d: Double => BigDecimal(d)
    case f: Float => BigDecimal(f)
    case _ => BigDecimal(0)

  /** Convert a typed integer value (placeholder). */
  def int(it: Any)(x: BigInt): Any = x // Placeholder

  /** Convert an int16 to a bigint. */
  def int16ToBigint(x: Short): BigInt = BigInt(x)

  /** Convert an int32 to a bigint. */
  def int32ToBigint(x: Int): BigInt = BigInt(x)

  /** Convert an int32 (from Byte) to a bigint. */
  def int32ToBigint(x: Byte): BigInt = BigInt(x.toInt)

  /** Convert an int64 to a bigint. */
  def int64ToBigint(x: Long): BigInt = BigInt(x)

  /** Convert an int8 to a bigint. */
  def int8ToBigint(x: Byte): BigInt = BigInt(x)

  /** Convert an integer value to a bigint. */
  def integerValueToBigint(x: Any): BigInt = x match
    case i: Int => BigInt(i)
    case l: Long => BigInt(l)
    case b: BigInt => b
    case _ => BigInt(0)

  /** Parse a string to a bigfloat. */
  def readBigfloat(s: String): Option[BigDecimal] = try Some(BigDecimal(s)) catch { case _: Exception => None }

  /** Parse a string to a bigint. */
  def readBigint(s: String): Option[BigInt] = try Some(BigInt(s)) catch { case _: Exception => None }

  /** Parse a string to a boolean. */
  def readBoolean(s: String): Option[Boolean] = s.toBooleanOption

  /** Parse a string to a float value (placeholder). */
  def readFloat(s: String)(ft: Any): Option[Any] = try Some(BigDecimal(s)) catch { case _: Exception => None }

  /** Parse a string to a float32. */
  def readFloat32(s: String): Option[Float] = s.toFloatOption

  /** Parse a string to a float64. */
  def readFloat64(s: String): Option[Double] = s.toDoubleOption

  /** Parse a string to an integer value (placeholder). */
  def readInt(s: String)(it: Any): Option[Any] = s.toIntOption

  /** Parse a string to an int16. */
  def readInt16(s: String): Option[Short] = s.toShortOption

  /** Parse a string to an int32. */
  def readInt32(s: String): Option[Int] = s.toIntOption

  /** Parse a string to an int64. */
  def readInt64(s: String): Option[Long] = s.toLongOption

  /** Parse a string to an int8. */
  def readInt8(s: String): Option[Byte] = s.toByteOption

  /** Parse a string literal. */
  def readString(s: String): Option[String] =
    // Haskell read expects a quoted string: read "\"hello\"" = "hello"
    if s.startsWith("\"") && s.endsWith("\"") && s.length >= 2 then
      Some(s.substring(1, s.length - 1)
        .replace("\\\\", "\u0000").replace("\\\"", "\"")
        .replace("\\n", "\n").replace("\\t", "\t").replace("\\r", "\r")
        .replace("\\a", "\u0007").replace("\\b", "\b").replace("\\f", "\f")
        .replace("\\v", "\u000b")
        .replace("\u0000", "\\"))
    else None

  /** Parse a string to an unsigned integer value (placeholder). */
  def readUint(s: String)(it: Any): Option[Any] = s.toIntOption.filter(_ >= 0)

  /** Parse a string to a uint16 (0 to 65535). */
  def readUint16(s: String): Option[Int] = s.toIntOption.filter(n => n >= 0 && n <= 65535)

  /** Parse a string to a uint32 (0 to 4294967295). */
  def readUint32(s: String): Option[Long] = s.toLongOption.filter(n => n >= 0 && n <= 4294967295L)

  /** Parse a string to a uint64 (0 to 18446744073709551615). */
  def readUint64(s: String): Option[BigInt] = try { val n = BigInt(s); if (n >= 0 && n <= BigInt("18446744073709551615")) Some(n) else None } catch { case _: Exception => None }

  /** Parse a string to a uint8 (0 to 255). */
  def readUint8(s: String): Option[Byte] = s.toShortOption.filter(n => n >= 0 && n <= 255).map(_.toByte)

  /** Convert a bigfloat to string. */
  def showBigfloat(x: BigDecimal): String = showHaskellDouble(x.toDouble)

  /** Convert a bigint to string. */
  def showBigint(x: BigInt): String = x.toString

  /** Convert a boolean to string. */
  def showBoolean(x: Boolean): String = if x then "true" else "false"

  /** Convert a float value to string (placeholder). */
  def showFloat(x: Any)(ft: Any): String = x.toString

  /** Convert a float32 to string. */
  def showFloat32(x: Float): String = showHaskellFloat32(x)

  /** Convert a float64 to string. */
  def showFloat64(x: Double): String = showHaskellDouble(x)

  /** Convert an integer value to string (placeholder). */
  def showInt(x: Any)(it: Any): String = x.toString

  /** Convert an int16 to string. */
  def showInt16(x: Short): String = x.toString

  /** Convert an int32 to string. */
  def showInt32(x: Int): String = x.toString

  /** Convert an int64 to string. */
  def showInt64(x: Long): String = x.toString

  /** Convert an int8 to string. */
  def showInt8(x: Byte): String = x.toString

  /** Convert a string to a quoted string representation. */
  def showString(s: String): String =
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

  /** Convert an unsigned integer value to string (placeholder). */
  def showUint(x: Any)(it: Any): String = x.toString

  /** Convert a uint16 to string. */
  def showUint16(x: Int): String = x.toString

  /** Convert a uint32 to string. */
  def showUint32(x: Long): String = x.toString

  /** Convert a uint64 to string. */
  def showUint64(x: BigInt): String = x.toString

  /** Convert a uint8 to string. */
  def showUint8(x: Byte): String = (x.toInt & 0xff).toString

  /** Convert string to binary by base64 decoding. */
  def stringToBinary(s: String): String = s

  /** Convert a typed unsigned integer value (placeholder). */
  def uint(it: Any)(x: BigInt): Any = x // Placeholder

  /** Convert a uint16 to a bigint. */
  def uint16ToBigint(x: Int): BigInt = BigInt(x)

  /** Convert a uint32 to a bigint. */
  def uint32ToBigint(x: Long): BigInt = BigInt(x)

  /** Convert a uint64 to a bigint. */
  def uint64ToBigint(x: BigInt): BigInt = x

  /** Convert a uint8 to a bigint. */
  def uint8ToBigint(x: Byte): BigInt = BigInt(x.toInt & 0xff)

  // Format a float32 to match Hydra's show for Float (Haskell-compatible)
  private def showHaskellFloat32(x: Float): String =
    if x.isNaN then "NaN"
    else if x.isInfinite then if x > 0 then "Infinity" else "-Infinity"
    else if x == 0.0f then if (java.lang.Float.floatToRawIntBits(x) < 0) "-0.0" else "0.0"
    else
      val abs = scala.math.abs(x)
      if abs >= 0.1f && abs < 1.0e7f then x.toString
      else toScientific(x.toString)

  // Format a double to match Hydra's show for Double (Haskell-compatible)
  private def showHaskellDouble(x: Double): String =
    if x.isNaN then "NaN"
    else if x.isInfinite then if x > 0 then "Infinity" else "-Infinity"
    else if x == 0.0 then if (java.lang.Double.doubleToRawLongBits(x) < 0) "-0.0" else "0.0"
    else
      val abs = scala.math.abs(x)
      if abs >= 0.1 && abs < 1.0e7 then x.toString
      else toScientific(x.toString)

  // Convert a Java float/double string representation to scientific notation
  private def toScientific(javaStr: String): String =
    val eIdx = javaStr.indexOf('E')
    if eIdx >= 0 then
      // Already in scientific notation, just lowercase the E
      javaStr.substring(0, eIdx) + "e" + javaStr.substring(eIdx + 1)
    else
      // Decimal form - convert to scientific notation
      val negative = javaStr.startsWith("-")
      val abs = if negative then javaStr.substring(1) else javaStr
      val dot = abs.indexOf('.')
      if dot < 0 then javaStr
      else
        val intPart = abs.substring(0, dot)
        val fracPart = abs.substring(dot + 1)
        val allDigits = intPart + fracPart
        // Find first non-zero digit
        var firstNonZero = 0
        while firstNonZero < allDigits.length && allDigits.charAt(firstNonZero) == '0' do firstNonZero += 1
        if firstNonZero >= allDigits.length then "0.0"
        else
          val exponent = dot - firstNonZero - 1
          var significant = allDigits.substring(firstNonZero)
          // Remove trailing zeros
          while significant.length > 1 && significant.endsWith("0") do
            significant = significant.substring(0, significant.length - 1)
          val mantissa = if significant.length == 1 then significant + ".0"
                         else significant.charAt(0).toString + "." + significant.substring(1)
          val sb = new StringBuilder
          if negative then sb.append('-')
          sb.append(mantissa).append('e').append(exponent)
          sb.toString
