package hydra.lib

object literals:
  // Haskell uses round (half-even / banker's rounding)
  def bigfloatToBigint(x: BigDecimal): BigInt = x.setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt
  def bigfloatToFloat(x: BigDecimal)(ft: Any): Any = ft // Placeholder
  def bigfloatToFloat32(x: BigDecimal): Float = x.toFloat
  def bigfloatToFloat64(x: BigDecimal): Double = x.toDouble
  def bigfloatToFloatValue(x: BigDecimal): Any = x // Placeholder
  def bigintToBigfloat(x: BigInt): BigDecimal = BigDecimal(x)
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
  def binaryToString(b: String): String = b
  def decimalToBigint(x: BigDecimal): BigInt = x.setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt
  def decimalToFloat32(x: BigDecimal): Float = x.toFloat
  def decimalToFloat64(x: BigDecimal): Double = x.toDouble
  def float(ft: Any)(x: BigDecimal): Any = x // Placeholder
  def float32ToBigfloat(x: Float): BigDecimal = BigDecimal(x.toDouble)
  def float32ToDecimal(x: Float): BigDecimal = BigDecimal(x.toString)
  def float64ToBigfloat(x: Double): BigDecimal = BigDecimal(x)
  def float64ToDecimal(x: Double): BigDecimal = BigDecimal(x.toString)
  def floatValueToBigfloat(x: Any): BigDecimal = x match
    case d: BigDecimal => d
    case d: Double => BigDecimal(d)
    case f: Float => BigDecimal(f)
    case _ => BigDecimal(0)
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
  def readBigfloat(s: String): Option[BigDecimal] = try Some(BigDecimal(s)) catch { case _: Exception => None }
  def readBigint(s: String): Option[BigInt] = try Some(BigInt(s)) catch { case _: Exception => None }
  def readBoolean(s: String): Option[Boolean] = s.toBooleanOption
  def readDecimal(s: String): Option[BigDecimal] = try Some(BigDecimal(s)) catch { case _: Exception => None }
  def readFloat(s: String)(ft: Any): Option[Any] = try Some(BigDecimal(s)) catch { case _: Exception => None }
  def readFloat32(s: String): Option[Float] = s.toFloatOption
  def readFloat64(s: String): Option[Double] = s.toDoubleOption
  def readInt(s: String)(it: Any): Option[Any] = s.toIntOption
  def readInt8(s: String): Option[Byte] = s.toByteOption
  def readInt16(s: String): Option[Short] = s.toShortOption
  def readInt32(s: String): Option[Int] = s.toIntOption
  def readInt64(s: String): Option[Long] = s.toLongOption
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
  def readUint(s: String)(it: Any): Option[Any] = s.toIntOption.filter(_ >= 0)
  def readUint8(s: String): Option[Byte] = s.toShortOption.filter(n => n >= 0 && n <= 255).map(_.toByte)
  def readUint16(s: String): Option[Int] = s.toIntOption.filter(n => n >= 0 && n <= 65535)
  def readUint32(s: String): Option[Long] = s.toLongOption.filter(n => n >= 0 && n <= 4294967295L)
  def readUint64(s: String): Option[BigInt] = try { val n = BigInt(s); if (n >= 0 && n <= BigInt("18446744073709551615")) Some(n) else None } catch { case _: Exception => None }

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

  def showBigfloat(x: BigDecimal): String = showHaskellDouble(x.toDouble)
  def showBigint(x: BigInt): String = x.toString
  def showBoolean(x: Boolean): String = if x then "true" else "false"
  def showDecimal(x: BigDecimal): String = {
    // Match Haskell's Data.Scientific show: "42.0", "3.14", "1.0e20", "1.0e-10".
    val stripped = x.underlying.stripTrailingZeros
    if (stripped.signum == 0) return "0.0"
    val precision = stripped.precision
    val scale = stripped.scale
    val e = precision - scale - 1
    val sign = if (stripped.signum < 0) "-" else ""
    val plain = stripped.unscaledValue.abs.toString
    // Haskell Scientific uses plain form iff -1 <= e <= 6; otherwise scientific.
    if (e >= 7 || e < -1) {
      val mantissa = if (plain.length == 1) plain + ".0" else plain.charAt(0).toString + "." + plain.substring(1)
      s"$sign${mantissa}e$e"
    } else {
      val s = stripped.toPlainString
      if (s.contains(".")) s else s + ".0"
    }
  }
  def showFloat(x: Any)(ft: Any): String = x.toString
  def showFloat32(x: Float): String = showHaskellFloat32(x)
  def showFloat64(x: Double): String = showHaskellDouble(x)
  def showInt(x: Any)(it: Any): String = x.toString
  def showInt8(x: Byte): String = x.toString
  def showInt16(x: Short): String = x.toString
  def showInt32(x: Int): String = x.toString
  def showInt64(x: Long): String = x.toString

  // Haskell show for strings: escapes special chars and non-ASCII
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

  def showUint(x: Any)(it: Any): String = x.toString
  def showUint8(x: Byte): String = (x.toInt & 0xff).toString
  def showUint16(x: Int): String = x.toString
  def showUint32(x: Long): String = x.toString
  def showUint64(x: BigInt): String = x.toString
  def stringToBinary(s: String): String = s
  def uint(it: Any)(x: BigInt): Any = x // Placeholder
  def uint8ToBigint(x: Byte): BigInt = BigInt(x.toInt & 0xff)
  def uint16ToBigint(x: Int): BigInt = BigInt(x)
  def uint32ToBigint(x: Long): BigInt = BigInt(x)
  def uint64ToBigint(x: BigInt): BigInt = x
