// Note: this is an automatically generated file. Do not edit.
// hydra.lib.literals primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class LiteralsTest extends AnyFunSuite {

  // bigintToInt8

  test("bigintToInt8 - positive") {

    assert((

      hydra.lib.literals.bigintToInt8(BigInt("42"))) == (

      42.toByte))

  }

  test("bigintToInt8 - negative") {

    assert((

      hydra.lib.literals.bigintToInt8(BigInt("-42"))) == (

      -42.toByte))

  }

  // bigintToInt16

  test("bigintToInt16 - positive") {

    assert((

      hydra.lib.literals.bigintToInt16(BigInt("1000"))) == (

      1000.toShort))

  }

  test("bigintToInt16 - negative") {

    assert((

      hydra.lib.literals.bigintToInt16(BigInt("-1000"))) == (

      -1000.toShort))

  }

  // bigintToInt32

  test("bigintToInt32 - positive") {

    assert((

      hydra.lib.literals.bigintToInt32(BigInt("42"))) == (

      42))

  }

  test("bigintToInt32 - negative") {

    assert((

      hydra.lib.literals.bigintToInt32(BigInt("-42"))) == (

      -42))

  }

  test("bigintToInt32 - zero") {

    assert((

      hydra.lib.literals.bigintToInt32(BigInt("0"))) == (

      0))

  }

  // bigintToInt64

  test("bigintToInt64 - positive") {

    assert((

      hydra.lib.literals.bigintToInt64(BigInt("1000000"))) == (

      1000000L))

  }

  test("bigintToInt64 - negative") {

    assert((

      hydra.lib.literals.bigintToInt64(BigInt("-1000000"))) == (

      -1000000L))

  }

  // bigintToUint8

  test("bigintToUint8 - zero") {

    assert((

      hydra.lib.literals.bigintToUint8(BigInt("0"))) == (

      0.toByte))

  }

  test("bigintToUint8 - typical value") {

    assert((

      hydra.lib.literals.bigintToUint8(BigInt("100"))) == (

      100.toByte))

  }

  // bigintToUint16

  test("bigintToUint16 - zero") {

    assert((

      hydra.lib.literals.bigintToUint16(BigInt("0"))) == (

      0))

  }

  test("bigintToUint16 - typical value") {

    assert((

      hydra.lib.literals.bigintToUint16(BigInt("1000"))) == (

      1000))

  }

  // bigintToUint32

  test("bigintToUint32 - zero") {

    assert((

      hydra.lib.literals.bigintToUint32(BigInt("0"))) == (

      0L))

  }

  test("bigintToUint32 - typical value") {

    assert((

      hydra.lib.literals.bigintToUint32(BigInt("100000"))) == (

      100000L))

  }

  // bigintToUint64

  test("bigintToUint64 - zero") {

    assert((

      hydra.lib.literals.bigintToUint64(BigInt("0"))) == (

      BigInt("0")))

  }

  test("bigintToUint64 - typical value") {

    assert((

      hydra.lib.literals.bigintToUint64(BigInt("1000000"))) == (

      BigInt("1000000")))

  }

  // int8ToBigint

  test("int8ToBigint - positive") {

    assert((

      hydra.lib.literals.int8ToBigint(42.toByte)) == (

      BigInt("42")))

  }

  test("int8ToBigint - negative") {

    assert((

      hydra.lib.literals.int8ToBigint(-42.toByte)) == (

      BigInt("-42")))

  }

  test("int8ToBigint - max value") {

    assert((

      hydra.lib.literals.int8ToBigint(127.toByte)) == (

      BigInt("127")))

  }

  test("int8ToBigint - min value") {

    assert((

      hydra.lib.literals.int8ToBigint(-128.toByte)) == (

      BigInt("-128")))

  }

  // int16ToBigint

  test("int16ToBigint - positive") {

    assert((

      hydra.lib.literals.int16ToBigint(1000.toShort)) == (

      BigInt("1000")))

  }

  test("int16ToBigint - negative") {

    assert((

      hydra.lib.literals.int16ToBigint(-1000.toShort)) == (

      BigInt("-1000")))

  }

  // int32ToBigint

  test("int32ToBigint - positive") {

    assert((

      hydra.lib.literals.int32ToBigint(42)) == (

      BigInt("42")))

  }

  test("int32ToBigint - negative") {

    assert((

      hydra.lib.literals.int32ToBigint(-42)) == (

      BigInt("-42")))

  }

  test("int32ToBigint - zero") {

    assert((

      hydra.lib.literals.int32ToBigint(0)) == (

      BigInt("0")))

  }

  // int64ToBigint

  test("int64ToBigint - positive") {

    assert((

      hydra.lib.literals.int64ToBigint(1000000L)) == (

      BigInt("1000000")))

  }

  test("int64ToBigint - negative") {

    assert((

      hydra.lib.literals.int64ToBigint(-1000000L)) == (

      BigInt("-1000000")))

  }

  // uint8ToBigint

  test("uint8ToBigint - zero") {

    assert((

      hydra.lib.literals.uint8ToBigint(0.toByte)) == (

      BigInt("0")))

  }

  test("uint8ToBigint - max value") {

    assert((

      hydra.lib.literals.uint8ToBigint(-1.toByte)) == (

      BigInt("255")))

  }

  // uint16ToBigint

  test("uint16ToBigint - zero") {

    assert((

      hydra.lib.literals.uint16ToBigint(0)) == (

      BigInt("0")))

  }

  test("uint16ToBigint - typical value") {

    assert((

      hydra.lib.literals.uint16ToBigint(1000)) == (

      BigInt("1000")))

  }

  // uint32ToBigint

  test("uint32ToBigint - zero") {

    assert((

      hydra.lib.literals.uint32ToBigint(0L)) == (

      BigInt("0")))

  }

  test("uint32ToBigint - typical value") {

    assert((

      hydra.lib.literals.uint32ToBigint(100000L)) == (

      BigInt("100000")))

  }

  // uint64ToBigint

  test("uint64ToBigint - zero") {

    assert((

      hydra.lib.literals.uint64ToBigint(BigInt("0"))) == (

      BigInt("0")))

  }

  test("uint64ToBigint - typical value") {

    assert((

      hydra.lib.literals.uint64ToBigint(BigInt("1000000"))) == (

      BigInt("1000000")))

  }

  // float32ToBigfloat

  test("float32ToBigfloat - positive") {

    assert((BigDecimal(2.5)).compare(hydra.lib.literals.float32ToBigfloat(2.5f)) == 0)

  }

  test("float32ToBigfloat - negative") {

    assert((BigDecimal(-2.5)).compare(hydra.lib.literals.float32ToBigfloat(-2.5f)) == 0)

  }

  test("float32ToBigfloat - zero") {

    assert((BigDecimal(0.0)).compare(hydra.lib.literals.float32ToBigfloat(0.0f)) == 0)

  }

  // float64ToBigfloat

  test("float64ToBigfloat - positive") {

    assert((BigDecimal(3.14159)).compare(hydra.lib.literals.float64ToBigfloat(3.14159)) == 0)

  }

  test("float64ToBigfloat - negative") {

    assert((BigDecimal(-2.71828)).compare(hydra.lib.literals.float64ToBigfloat(-2.71828)) == 0)

  }

  test("float64ToBigfloat - zero") {

    assert((BigDecimal(0.0)).compare(hydra.lib.literals.float64ToBigfloat(0.0)) == 0)

  }

  // bigfloatToFloat32

  test("bigfloatToFloat32 - positive") {

    assert(math.abs((hydra.lib.literals.bigfloatToFloat32(BigDecimal(3.14))) - (3.14f)) <= 1e-15)

  }

  test("bigfloatToFloat32 - negative") {

    assert(math.abs((hydra.lib.literals.bigfloatToFloat32(BigDecimal(-2.5))) - (-2.5f)) <= 1e-15)

  }

  test("bigfloatToFloat32 - zero") {

    assert(math.abs((hydra.lib.literals.bigfloatToFloat32(BigDecimal(0.0))) - (0.0f)) <= 1e-15)

  }

  // bigfloatToFloat64

  test("bigfloatToFloat64 - positive") {

    assert(math.abs((hydra.lib.literals.bigfloatToFloat64(BigDecimal(3.14159))) - (3.14159)) <= 1e-15)

  }

  test("bigfloatToFloat64 - negative") {

    assert(math.abs((hydra.lib.literals.bigfloatToFloat64(BigDecimal(-2.71828))) - (-2.71828)) <= 1e-15)

  }

  test("bigfloatToFloat64 - zero") {

    assert(math.abs((hydra.lib.literals.bigfloatToFloat64(BigDecimal(0.0))) - (0.0)) <= 1e-15)

  }

  // bigintToBigfloat

  test("bigintToBigfloat - positive") {

    assert((BigDecimal(42.0)).compare(hydra.lib.literals.bigintToBigfloat(BigInt("42"))) == 0)

  }

  test("bigintToBigfloat - negative") {

    assert((BigDecimal(-42.0)).compare(hydra.lib.literals.bigintToBigfloat(BigInt("-42"))) == 0)

  }

  test("bigintToBigfloat - zero") {

    assert((BigDecimal(0.0)).compare(hydra.lib.literals.bigintToBigfloat(BigInt("0"))) == 0)

  }

  // bigfloatToBigint

  test("bigfloatToBigint - positive") {

    assert((

      hydra.lib.literals.bigfloatToBigint(BigDecimal(42.7))) == (

      BigInt("43")))

  }

  test("bigfloatToBigint - negative") {

    assert((

      hydra.lib.literals.bigfloatToBigint(BigDecimal(-42.7))) == (

      BigInt("-43")))

  }

  test("bigfloatToBigint - zero") {

    assert((

      hydra.lib.literals.bigfloatToBigint(BigDecimal(0.0))) == (

      BigInt("0")))

  }

  test("bigfloatToBigint - round down") {

    assert((

      hydra.lib.literals.bigfloatToBigint(BigDecimal(42.3))) == (

      BigInt("42")))

  }

  test("bigfloatToBigint - half even up") {

    assert((

      hydra.lib.literals.bigfloatToBigint(BigDecimal(42.5))) == (

      BigInt("42")))

  }

  test("bigfloatToBigint - half even down") {

    assert((

      hydra.lib.literals.bigfloatToBigint(BigDecimal(43.5))) == (

      BigInt("44")))

  }

  // showInt8

  test("showInt8 - positive") {

    assert((

      hydra.lib.literals.showInt8(42.toByte)) == (

      "42"))

  }

  test("showInt8 - negative") {

    assert((

      hydra.lib.literals.showInt8(-42.toByte)) == (

      "-42"))

  }

  // showInt16

  test("showInt16 - positive") {

    assert((

      hydra.lib.literals.showInt16(1000.toShort)) == (

      "1000"))

  }

  test("showInt16 - negative") {

    assert((

      hydra.lib.literals.showInt16(-1000.toShort)) == (

      "-1000"))

  }

  // showInt32

  test("showInt32 - positive") {

    assert((

      hydra.lib.literals.showInt32(42)) == (

      "42"))

  }

  test("showInt32 - negative") {

    assert((

      hydra.lib.literals.showInt32(-42)) == (

      "-42"))

  }

  test("showInt32 - zero") {

    assert((

      hydra.lib.literals.showInt32(0)) == (

      "0"))

  }

  // showInt64

  test("showInt64 - positive") {

    assert((

      hydra.lib.literals.showInt64(1000000L)) == (

      "1000000"))

  }

  test("showInt64 - negative") {

    assert((

      hydra.lib.literals.showInt64(-1000000L)) == (

      "-1000000"))

  }

  // showUint8

  test("showUint8 - zero") {

    assert((

      hydra.lib.literals.showUint8(0.toByte)) == (

      "0"))

  }

  test("showUint8 - max value") {

    assert((

      hydra.lib.literals.showUint8(-1.toByte)) == (

      "255"))

  }

  // showUint16

  test("showUint16 - zero") {

    assert((

      hydra.lib.literals.showUint16(0)) == (

      "0"))

  }

  test("showUint16 - typical value") {

    assert((

      hydra.lib.literals.showUint16(1000)) == (

      "1000"))

  }

  // showUint32

  test("showUint32 - zero") {

    assert((

      hydra.lib.literals.showUint32(0L)) == (

      "0"))

  }

  test("showUint32 - typical value") {

    assert((

      hydra.lib.literals.showUint32(100000L)) == (

      "100000"))

  }

  // showUint64

  test("showUint64 - zero") {

    assert((

      hydra.lib.literals.showUint64(BigInt("0"))) == (

      "0"))

  }

  test("showUint64 - typical value") {

    assert((

      hydra.lib.literals.showUint64(BigInt("1000000"))) == (

      "1000000"))

  }

  // showBigint

  test("showBigint - positive") {

    assert((

      hydra.lib.literals.showBigint(BigInt("42"))) == (

      "42"))

  }

  test("showBigint - negative") {

    assert((

      hydra.lib.literals.showBigint(BigInt("-42"))) == (

      "-42"))

  }

  test("showBigint - zero") {

    assert((

      hydra.lib.literals.showBigint(BigInt("0"))) == (

      "0"))

  }

  // showFloat32

  test("showFloat32 - positive") {

    assert((

      hydra.lib.literals.showFloat32(3.14f)) == (

      "3.14"))

  }

  test("showFloat32 - negative") {

    assert((

      hydra.lib.literals.showFloat32(-2.5f)) == (

      "-2.5"))

  }

  test("showFloat32 - zero") {

    assert((

      hydra.lib.literals.showFloat32(0.0f)) == (

      "0.0"))

  }

  test("showFloat32 - small positive") {

    assert((

      hydra.lib.literals.showFloat32(5.0e-2f)) == (

      "5.0e-2"))

  }

  test("showFloat32 - small positive 2") {

    assert((

      hydra.lib.literals.showFloat32(3.0e-2f)) == (

      "3.0e-2"))

  }

  test("showFloat32 - very small") {

    assert((

      hydra.lib.literals.showFloat32(1.0e-3f)) == (

      "1.0e-3"))

  }

  test("showFloat32 - normal decimal") {

    assert((

      hydra.lib.literals.showFloat32(0.1f)) == (

      "0.1"))

  }

  // showFloat64

  test("showFloat64 - positive") {

    assert((

      hydra.lib.literals.showFloat64(3.14159)) == (

      "3.14159"))

  }

  test("showFloat64 - zero") {

    assert((

      hydra.lib.literals.showFloat64(0.0)) == (

      "0.0"))

  }

  test("showFloat64 - small positive") {

    assert((

      hydra.lib.literals.showFloat64(5.0e-2)) == (

      "5.0e-2"))

  }

  test("showFloat64 - small positive 2") {

    assert((

      hydra.lib.literals.showFloat64(3.0e-2)) == (

      "3.0e-2"))

  }

  test("showFloat64 - very small") {

    assert((

      hydra.lib.literals.showFloat64(1.0e-3)) == (

      "1.0e-3"))

  }

  test("showFloat64 - normal decimal") {

    assert((

      hydra.lib.literals.showFloat64(0.1)) == (

      "0.1"))

  }

  // showBigfloat

  test("showBigfloat - positive") {

    assert((

      hydra.lib.literals.showBigfloat(BigDecimal(3.14))) == (

      "3.14"))

  }

  test("showBigfloat - zero") {

    assert((

      hydra.lib.literals.showBigfloat(BigDecimal(0.0))) == (

      "0.0"))

  }

  test("showBigfloat - small positive") {

    assert((

      hydra.lib.literals.showBigfloat(BigDecimal(5.0e-2))) == (

      "5.0e-2"))

  }

  test("showBigfloat - small positive 2") {

    assert((

      hydra.lib.literals.showBigfloat(BigDecimal(3.0e-2))) == (

      "3.0e-2"))

  }

  test("showBigfloat - very small") {

    assert((

      hydra.lib.literals.showBigfloat(BigDecimal(1.0e-3))) == (

      "1.0e-3"))

  }

  test("showBigfloat - normal decimal") {

    assert((

      hydra.lib.literals.showBigfloat(BigDecimal(0.1))) == (

      "0.1"))

  }

  // showBoolean

  test("showBoolean - true") {

    assert((

      hydra.lib.literals.showBoolean(true)) == (

      "true"))

  }

  test("showBoolean - false") {

    assert((

      hydra.lib.literals.showBoolean(false)) == (

      "false"))

  }

  // showString

  test("showString - simple") {

    assert((

      hydra.lib.literals.showString("hello")) == (

      "\"hello\""))

  }

  test("showString - empty") {

    assert((

      hydra.lib.literals.showString("")) == (

      "\"\""))

  }

  test("showString - latin accented") {

    assert((

      hydra.lib.literals.showString("caf\u00E9")) == (

      "\"caf\\233\""))

  }

  test("showString - greek lambda") {

    assert((

      hydra.lib.literals.showString("\u03BB")) == (

      "\"\\955\""))

  }

  test("showString - mixed ascii and non-ascii") {

    assert((

      hydra.lib.literals.showString("A\u00E9B")) == (

      "\"A\\233B\""))

  }

  test("showString - tab") {

    assert((

      hydra.lib.literals.showString("\t")) == (

      "\"\\t\""))

  }

  test("showString - newline") {

    assert((

      hydra.lib.literals.showString("\n")) == (

      "\"\\n\""))

  }

  test("showString - carriage return") {

    assert((

      hydra.lib.literals.showString("\r")) == (

      "\"\\r\""))

  }

  test("showString - backslash") {

    assert((

      hydra.lib.literals.showString("\\")) == (

      "\"\\\\\""))

  }

  test("showString - double quote") {

    assert((

      hydra.lib.literals.showString("\"")) == (

      "\"\\\"\""))

  }

  test("showString - null") {

    assert((

      hydra.lib.literals.showString("\u0000")) == (

      "\"\\NUL\""))

  }

  test("showString - bell") {

    assert((

      hydra.lib.literals.showString("\u0007")) == (

      "\"\\a\""))

  }

  test("showString - backspace") {

    assert((

      hydra.lib.literals.showString("\b")) == (

      "\"\\b\""))

  }

  test("showString - form feed") {

    assert((

      hydra.lib.literals.showString("\f")) == (

      "\"\\f\""))

  }

  test("showString - vertical tab") {

    assert((

      hydra.lib.literals.showString("\u000B")) == (

      "\"\\v\""))

  }

  test("showString - delete") {

    assert((

      hydra.lib.literals.showString("\u007F")) == (

      "\"\\DEL\""))

  }

  // readInt8

  test("readInt8 - positive") {

    assert((

      hydra.lib.literals.readInt8("42")) == (

      Some(42.toByte)))

  }

  test("readInt8 - negative") {

    assert((

      hydra.lib.literals.readInt8("-42")) == (

      Some(-42.toByte)))

  }

  test("readInt8 - max value") {

    assert((

      hydra.lib.literals.readInt8("127")) == (

      Some(127.toByte)))

  }

  test("readInt8 - min value") {

    assert((

      hydra.lib.literals.readInt8("-128")) == (

      Some(-128.toByte)))

  }

  test("readInt8 - invalid") {

    assert((

      hydra.lib.literals.readInt8("abc")) == (

      None))

  }

  test("readInt8 - overflow") {

    assert((

      hydra.lib.literals.readInt8("128")) == (

      None))

  }

  // readInt16

  test("readInt16 - positive") {

    assert((

      hydra.lib.literals.readInt16("1000")) == (

      Some(1000.toShort)))

  }

  test("readInt16 - negative") {

    assert((

      hydra.lib.literals.readInt16("-1000")) == (

      Some(-1000.toShort)))

  }

  test("readInt16 - invalid") {

    assert((

      hydra.lib.literals.readInt16("abc")) == (

      None))

  }

  // readInt32

  test("readInt32 - positive") {

    assert((

      hydra.lib.literals.readInt32("42")) == (

      Some(42)))

  }

  test("readInt32 - negative") {

    assert((

      hydra.lib.literals.readInt32("-42")) == (

      Some(-42)))

  }

  test("readInt32 - invalid") {

    assert((

      hydra.lib.literals.readInt32("abc")) == (

      None))

  }

  // readInt64

  test("readInt64 - positive") {

    assert((

      hydra.lib.literals.readInt64("1000000")) == (

      Some(1000000L)))

  }

  test("readInt64 - negative") {

    assert((

      hydra.lib.literals.readInt64("-1000000")) == (

      Some(-1000000L)))

  }

  test("readInt64 - invalid") {

    assert((

      hydra.lib.literals.readInt64("abc")) == (

      None))

  }

  // readUint8

  test("readUint8 - zero") {

    assert((

      hydra.lib.literals.readUint8("0")) == (

      Some(0.toByte)))

  }

  test("readUint8 - typical") {

    assert((

      hydra.lib.literals.readUint8("100")) == (

      Some(100.toByte)))

  }

  test("readUint8 - max value") {

    assert((

      hydra.lib.literals.readUint8("255")) == (

      Some(-1.toByte)))

  }

  test("readUint8 - invalid") {

    assert((

      hydra.lib.literals.readUint8("abc")) == (

      None))

  }

  test("readUint8 - negative") {

    assert((

      hydra.lib.literals.readUint8("-1")) == (

      None))

  }

  // readUint16

  test("readUint16 - zero") {

    assert((

      hydra.lib.literals.readUint16("0")) == (

      Some(0)))

  }

  test("readUint16 - typical") {

    assert((

      hydra.lib.literals.readUint16("1000")) == (

      Some(1000)))

  }

  test("readUint16 - invalid") {

    assert((

      hydra.lib.literals.readUint16("abc")) == (

      None))

  }

  test("readUint16 - negative") {

    assert((

      hydra.lib.literals.readUint16("-1")) == (

      None))

  }

  // readUint32

  test("readUint32 - zero") {

    assert((

      hydra.lib.literals.readUint32("0")) == (

      Some(0L)))

  }

  test("readUint32 - typical") {

    assert((

      hydra.lib.literals.readUint32("100000")) == (

      Some(100000L)))

  }

  test("readUint32 - invalid") {

    assert((

      hydra.lib.literals.readUint32("abc")) == (

      None))

  }

  test("readUint32 - negative") {

    assert((

      hydra.lib.literals.readUint32("-1")) == (

      None))

  }

  // readUint64

  test("readUint64 - zero") {

    assert((

      hydra.lib.literals.readUint64("0")) == (

      Some(BigInt("0"))))

  }

  test("readUint64 - typical") {

    assert((

      hydra.lib.literals.readUint64("1000000")) == (

      Some(BigInt("1000000"))))

  }

  test("readUint64 - invalid") {

    assert((

      hydra.lib.literals.readUint64("abc")) == (

      None))

  }

  test("readUint64 - negative") {

    assert((

      hydra.lib.literals.readUint64("-1")) == (

      None))

  }

  // readBigint

  test("readBigint - positive") {

    assert((

      hydra.lib.literals.readBigint("42")) == (

      Some(BigInt("42"))))

  }

  test("readBigint - negative") {

    assert((

      hydra.lib.literals.readBigint("-42")) == (

      Some(BigInt("-42"))))

  }

  test("readBigint - zero") {

    assert((

      hydra.lib.literals.readBigint("0")) == (

      Some(BigInt("0"))))

  }

  test("readBigint - large") {

    assert((

      hydra.lib.literals.readBigint("123456789012345678901234567890")) == (

      Some(BigInt("123456789012345678901234567890"))))

  }

  test("readBigint - invalid") {

    assert((

      hydra.lib.literals.readBigint("abc")) == (

      None))

  }

  // readFloat32

  test("readFloat32 - positive") {

    assert((

      hydra.lib.literals.readFloat32("3.14")) == (

      Some(3.14f)))

  }

  test("readFloat32 - negative") {

    assert((

      hydra.lib.literals.readFloat32("-2.5")) == (

      Some(-2.5f)))

  }

  test("readFloat32 - invalid") {

    assert((

      hydra.lib.literals.readFloat32("abc")) == (

      None))

  }

  // readFloat64

  test("readFloat64 - positive") {

    assert((

      hydra.lib.literals.readFloat64("3.14159")) == (

      Some(3.14159)))

  }

  test("readFloat64 - negative") {

    assert((

      hydra.lib.literals.readFloat64("-2.71828")) == (

      Some(-2.71828)))

  }

  test("readFloat64 - invalid") {

    assert((

      hydra.lib.literals.readFloat64("abc")) == (

      None))

  }

  // readBigfloat

  test("readBigfloat - positive") {

    assert((

      hydra.lib.literals.readBigfloat("3.14")) == (

      Some(BigDecimal(3.14))))

  }

  test("readBigfloat - invalid") {

    assert((

      hydra.lib.literals.readBigfloat("abc")) == (

      None))

  }

  // readBoolean

  test("readBoolean - true") {

    assert((

      hydra.lib.literals.readBoolean("true")) == (

      Some(true)))

  }

  test("readBoolean - false") {

    assert((

      hydra.lib.literals.readBoolean("false")) == (

      Some(false)))

  }

  test("readBoolean - invalid") {

    assert((

      hydra.lib.literals.readBoolean("yes")) == (

      None))

  }

  // readString

  test("readString - quoted string") {

    assert((

      hydra.lib.literals.readString("\"hello\"")) == (

      Some("hello")))

  }

  test("readString - empty quoted") {

    assert((

      hydra.lib.literals.readString("\"\"")) == (

      Some("")))

  }

  test("readString - unquoted") {

    assert((

      hydra.lib.literals.readString("hello")) == (

      None))

  }

  // stringToBinary

  test("stringToBinary - simple base64") {

    assert((

      hydra.lib.literals.stringToBinary("aGVsbG8=")) == (

      "aGVsbG8="))

  }

  test("stringToBinary - empty string") {

    assert((

      hydra.lib.literals.stringToBinary("")) == (

      ""))

  }

  // binaryToString

  test("binaryToString - simple binary") {

    assert((

      hydra.lib.literals.binaryToString("aGVsbG8=")) == (

      "aGVsbG8="))

  }

  test("binaryToString - empty binary") {

    assert((

      hydra.lib.literals.binaryToString("")) == (

      ""))

  }
}
