package hydra.lib

object literals:
  def bigfloatToBigint(x: BigDecimal): BigInt = x.toBigInt
  def bigfloatToFloat(x: BigDecimal)(ft: Any): Any = ft // Placeholder
  def bigfloatToFloat32(x: BigDecimal): Float = x.toFloat
  def bigfloatToFloat64(x: BigDecimal): Double = x.toDouble
  def bigfloatToFloatValue(x: BigDecimal): Any = x // Placeholder
  def bigintToBigfloat(x: BigInt): BigDecimal = BigDecimal(x)
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
  def binaryToBytes(b: String): Seq[Int] = b.getBytes("ISO-8859-1").toSeq.map(_.toInt & 0xff)
  def binaryToString(b: String): String = java.util.Base64.getEncoder.encodeToString(b.getBytes("ISO-8859-1"))
  def float(ft: Any)(x: BigDecimal): Any = x // Placeholder
  def float32ToBigfloat(x: Float): BigDecimal = BigDecimal(x)
  def float64ToBigfloat(x: Double): BigDecimal = BigDecimal(x)
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
  def readFloat(s: String)(ft: Any): Option[Any] = try Some(BigDecimal(s)) catch { case _: Exception => None }
  def readFloat32(s: String): Option[Float] = s.toFloatOption
  def readFloat64(s: String): Option[Double] = s.toDoubleOption
  def readInt(s: String)(it: Any): Option[Any] = s.toIntOption
  def readInt8(s: String): Option[Byte] = s.toByteOption
  def readInt16(s: String): Option[Short] = s.toShortOption
  def readInt32(s: String): Option[Int] = s.toIntOption
  def readInt64(s: String): Option[Long] = s.toLongOption
  def readString(s: String): Option[String] = Some(s)
  def readUint(s: String)(it: Any): Option[Any] = s.toIntOption.filter(_ >= 0)
  def readUint8(s: String): Option[Byte] = s.toShortOption.filter(n => n >= 0 && n <= 255).map(_.toByte)
  def readUint16(s: String): Option[Int] = s.toIntOption.filter(n => n >= 0 && n <= 65535)
  def readUint32(s: String): Option[Long] = s.toLongOption.filter(n => n >= 0 && n <= 4294967295L)
  def readUint64(s: String): Option[BigInt] = try { val n = BigInt(s); if (n >= 0 && n <= BigInt("18446744073709551615")) Some(n) else None } catch { case _: Exception => None }
  def showBigfloat(x: BigDecimal): String = x.toString
  def showBigint(x: BigInt): String = x.toString
  def showBoolean(x: Boolean): String = x.toString
  def showFloat(x: Any)(ft: Any): String = x.toString
  def showFloat32(x: Float): String = x.toString
  def showFloat64(x: Double): String = x.toString
  def showInt(x: Any)(it: Any): String = x.toString
  def showInt8(x: Byte): String = x.toString
  def showInt16(x: Short): String = x.toString
  def showInt32(x: Int): String = x.toString
  def showInt64(x: Long): String = x.toString
  def showString(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  def showUint(x: Any)(it: Any): String = x.toString
  def showUint8(x: Byte): String = x.toString
  def showUint16(x: Int): String = x.toString
  def showUint32(x: Long): String = x.toString
  def showUint64(x: BigInt): String = x.toString
  def stringToBinary(s: String): String = new String(java.util.Base64.getDecoder.decode(s), "ISO-8859-1")
  def uint(it: Any)(x: BigInt): Any = x // Placeholder
  def uint8ToBigint(x: Byte): BigInt = BigInt(x.toInt & 0xff)
  def uint16ToBigint(x: Int): BigInt = BigInt(x)
  def uint32ToBigint(x: Long): BigInt = BigInt(x)
  def uint64ToBigint(x: BigInt): BigInt = x
