// Note: this is an automatically generated file. Do not edit.
// hydra.lib.literals primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class LiteralsTest extends AnyFunSuite {

  // bigintToInt8

  test("bigintToInt8 - positive") {

    assert((

      42:int8) == (

      42:int8))

  }

  test("bigintToInt8 - negative") {

    assert((

      -42:int8) == (

      -42:int8))

  }

  // bigintToInt16

  test("bigintToInt16 - positive") {

    assert((

      1000:int16) == (

      1000:int16))

  }

  test("bigintToInt16 - negative") {

    assert((

      -1000:int16) == (

      -1000:int16))

  }

  // bigintToInt32

  test("bigintToInt32 - positive") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("bigintToInt32 - negative") {

    assert((

      -42:int32) == (

      -42:int32))

  }

  test("bigintToInt32 - zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  // bigintToInt64

  test("bigintToInt64 - positive") {

    assert((

      1000000:int64) == (

      1000000:int64))

  }

  test("bigintToInt64 - negative") {

    assert((

      -1000000:int64) == (

      -1000000:int64))

  }

  // bigintToUint8

  test("bigintToUint8 - zero") {

    assert((

      0:uint8) == (

      0:uint8))

  }

  test("bigintToUint8 - typical value") {

    assert((

      100:uint8) == (

      100:uint8))

  }

  // bigintToUint16

  test("bigintToUint16 - zero") {

    assert((

      0:uint16) == (

      0:uint16))

  }

  test("bigintToUint16 - typical value") {

    assert((

      1000:uint16) == (

      1000:uint16))

  }

  // bigintToUint32

  test("bigintToUint32 - zero") {

    assert((

      0:uint32) == (

      0:uint32))

  }

  test("bigintToUint32 - typical value") {

    assert((

      100000:uint32) == (

      100000:uint32))

  }

  // bigintToUint64

  test("bigintToUint64 - zero") {

    assert((

      0:uint64) == (

      0:uint64))

  }

  test("bigintToUint64 - typical value") {

    assert((

      1000000:uint64) == (

      1000000:uint64))

  }

  // int8ToBigint

  test("int8ToBigint - positive") {

    assert((

      42:bigint) == (

      42:bigint))

  }

  test("int8ToBigint - negative") {

    assert((

      -42:bigint) == (

      -42:bigint))

  }

  test("int8ToBigint - max value") {

    assert((

      127:bigint) == (

      127:bigint))

  }

  test("int8ToBigint - min value") {

    assert((

      -128:bigint) == (

      -128:bigint))

  }

  // int16ToBigint

  test("int16ToBigint - positive") {

    assert((

      1000:bigint) == (

      1000:bigint))

  }

  test("int16ToBigint - negative") {

    assert((

      -1000:bigint) == (

      -1000:bigint))

  }

  // int32ToBigint

  test("int32ToBigint - positive") {

    assert((

      42:bigint) == (

      42:bigint))

  }

  test("int32ToBigint - negative") {

    assert((

      -42:bigint) == (

      -42:bigint))

  }

  test("int32ToBigint - zero") {

    assert((

      0:bigint) == (

      0:bigint))

  }

  // int64ToBigint

  test("int64ToBigint - positive") {

    assert((

      1000000:bigint) == (

      1000000:bigint))

  }

  test("int64ToBigint - negative") {

    assert((

      -1000000:bigint) == (

      -1000000:bigint))

  }

  // uint8ToBigint

  test("uint8ToBigint - zero") {

    assert((

      0:bigint) == (

      0:bigint))

  }

  test("uint8ToBigint - max value") {

    assert((

      255:bigint) == (

      255:bigint))

  }

  // uint16ToBigint

  test("uint16ToBigint - zero") {

    assert((

      0:bigint) == (

      0:bigint))

  }

  test("uint16ToBigint - typical value") {

    assert((

      1000:bigint) == (

      1000:bigint))

  }

  // uint32ToBigint

  test("uint32ToBigint - zero") {

    assert((

      0:bigint) == (

      0:bigint))

  }

  test("uint32ToBigint - typical value") {

    assert((

      100000:bigint) == (

      100000:bigint))

  }

  // uint64ToBigint

  test("uint64ToBigint - zero") {

    assert((

      0:bigint) == (

      0:bigint))

  }

  test("uint64ToBigint - typical value") {

    assert((

      1000000:bigint) == (

      1000000:bigint))

  }

  // float32ToBigfloat

  test("float32ToBigfloat - positive") {

    assert((

      2.5:bigfloat) == (

      2.5:bigfloat))

  }

  test("float32ToBigfloat - negative") {

    assert((

      -2.5:bigfloat) == (

      -2.5:bigfloat))

  }

  test("float32ToBigfloat - zero") {

    assert((

      0.0:bigfloat) == (

      0.0:bigfloat))

  }

  // float64ToBigfloat

  test("float64ToBigfloat - positive") {

    assert((

      3.14159:bigfloat) == (

      3.14159:bigfloat))

  }

  test("float64ToBigfloat - negative") {

    assert((

      -2.71828:bigfloat) == (

      -2.71828:bigfloat))

  }

  test("float64ToBigfloat - zero") {

    assert((

      0.0:bigfloat) == (

      0.0:bigfloat))

  }

  // bigfloatToFloat32

  test("bigfloatToFloat32 - positive") {

    assert((

      3.14:float32) == (

      3.14:float32))

  }

  test("bigfloatToFloat32 - negative") {

    assert((

      -2.5:float32) == (

      -2.5:float32))

  }

  test("bigfloatToFloat32 - zero") {

    assert((

      0.0:float32) == (

      0.0:float32))

  }

  // bigfloatToFloat64

  test("bigfloatToFloat64 - positive") {

    assert((

      3.14159:float64) == (

      3.14159:float64))

  }

  test("bigfloatToFloat64 - negative") {

    assert((

      -2.71828:float64) == (

      -2.71828:float64))

  }

  test("bigfloatToFloat64 - zero") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  // bigintToBigfloat

  test("bigintToBigfloat - positive") {

    assert((

      42.0:bigfloat) == (

      42.0:bigfloat))

  }

  test("bigintToBigfloat - negative") {

    assert((

      -42.0:bigfloat) == (

      -42.0:bigfloat))

  }

  test("bigintToBigfloat - zero") {

    assert((

      0.0:bigfloat) == (

      0.0:bigfloat))

  }

  // bigfloatToBigint

  test("bigfloatToBigint - positive") {

    assert((

      43:bigint) == (

      43:bigint))

  }

  test("bigfloatToBigint - negative") {

    assert((

      -43:bigint) == (

      -43:bigint))

  }

  test("bigfloatToBigint - zero") {

    assert((

      0:bigint) == (

      0:bigint))

  }

  test("bigfloatToBigint - round down") {

    assert((

      42:bigint) == (

      42:bigint))

  }

  test("bigfloatToBigint - half even up") {

    assert((

      42:bigint) == (

      42:bigint))

  }

  test("bigfloatToBigint - half even down") {

    assert((

      44:bigint) == (

      44:bigint))

  }

  // showInt8

  test("showInt8 - positive") {

    assert((

      "42") == (

      "42"))

  }

  test("showInt8 - negative") {

    assert((

      "-42") == (

      "-42"))

  }

  // showInt16

  test("showInt16 - positive") {

    assert((

      "1000") == (

      "1000"))

  }

  test("showInt16 - negative") {

    assert((

      "-1000") == (

      "-1000"))

  }

  // showInt32

  test("showInt32 - positive") {

    assert((

      "42") == (

      "42"))

  }

  test("showInt32 - negative") {

    assert((

      "-42") == (

      "-42"))

  }

  test("showInt32 - zero") {

    assert((

      "0") == (

      "0"))

  }

  // showInt64

  test("showInt64 - positive") {

    assert((

      "1000000") == (

      "1000000"))

  }

  test("showInt64 - negative") {

    assert((

      "-1000000") == (

      "-1000000"))

  }

  // showUint8

  test("showUint8 - zero") {

    assert((

      "0") == (

      "0"))

  }

  test("showUint8 - max value") {

    assert((

      "255") == (

      "255"))

  }

  // showUint16

  test("showUint16 - zero") {

    assert((

      "0") == (

      "0"))

  }

  test("showUint16 - typical value") {

    assert((

      "1000") == (

      "1000"))

  }

  // showUint32

  test("showUint32 - zero") {

    assert((

      "0") == (

      "0"))

  }

  test("showUint32 - typical value") {

    assert((

      "100000") == (

      "100000"))

  }

  // showUint64

  test("showUint64 - zero") {

    assert((

      "0") == (

      "0"))

  }

  test("showUint64 - typical value") {

    assert((

      "1000000") == (

      "1000000"))

  }

  // showBigint

  test("showBigint - positive") {

    assert((

      "42") == (

      "42"))

  }

  test("showBigint - negative") {

    assert((

      "-42") == (

      "-42"))

  }

  test("showBigint - zero") {

    assert((

      "0") == (

      "0"))

  }

  // showFloat32

  test("showFloat32 - positive") {

    assert((

      "3.14") == (

      "3.14"))

  }

  test("showFloat32 - negative") {

    assert((

      "-2.5") == (

      "-2.5"))

  }

  test("showFloat32 - zero") {

    assert((

      "0.0") == (

      "0.0"))

  }

  test("showFloat32 - small positive") {

    assert((

      "5.0e-2") == (

      "5.0e-2"))

  }

  test("showFloat32 - small positive 2") {

    assert((

      "3.0e-2") == (

      "3.0e-2"))

  }

  test("showFloat32 - very small") {

    assert((

      "1.0e-3") == (

      "1.0e-3"))

  }

  test("showFloat32 - normal decimal") {

    assert((

      "0.1") == (

      "0.1"))

  }

  // showFloat64

  test("showFloat64 - positive") {

    assert((

      "3.14159") == (

      "3.14159"))

  }

  test("showFloat64 - zero") {

    assert((

      "0.0") == (

      "0.0"))

  }

  test("showFloat64 - small positive") {

    assert((

      "5.0e-2") == (

      "5.0e-2"))

  }

  test("showFloat64 - small positive 2") {

    assert((

      "3.0e-2") == (

      "3.0e-2"))

  }

  test("showFloat64 - very small") {

    assert((

      "1.0e-3") == (

      "1.0e-3"))

  }

  test("showFloat64 - normal decimal") {

    assert((

      "0.1") == (

      "0.1"))

  }

  // showBigfloat

  test("showBigfloat - positive") {

    assert((

      "3.14") == (

      "3.14"))

  }

  test("showBigfloat - zero") {

    assert((

      "0.0") == (

      "0.0"))

  }

  test("showBigfloat - small positive") {

    assert((

      "5.0e-2") == (

      "5.0e-2"))

  }

  test("showBigfloat - small positive 2") {

    assert((

      "3.0e-2") == (

      "3.0e-2"))

  }

  test("showBigfloat - very small") {

    assert((

      "1.0e-3") == (

      "1.0e-3"))

  }

  test("showBigfloat - normal decimal") {

    assert((

      "0.1") == (

      "0.1"))

  }

  // showBoolean

  test("showBoolean - true") {

    assert((

      "true") == (

      "true"))

  }

  test("showBoolean - false") {

    assert((

      "false") == (

      "false"))

  }

  // showString

  test("showString - simple") {

    assert((

      "\"hello\"") == (

      "\"hello\""))

  }

  test("showString - empty") {

    assert((

      "\"\"") == (

      "\"\""))

  }

  test("showString - latin accented") {

    assert((

      "\"caf\\233\"") == (

      "\"caf\\233\""))

  }

  test("showString - greek lambda") {

    assert((

      "\"\\955\"") == (

      "\"\\955\""))

  }

  test("showString - mixed ascii and non-ascii") {

    assert((

      "\"A\\233B\"") == (

      "\"A\\233B\""))

  }

  test("showString - tab") {

    assert((

      "\"\\t\"") == (

      "\"\\t\""))

  }

  test("showString - newline") {

    assert((

      "\"\\n\"") == (

      "\"\\n\""))

  }

  test("showString - carriage return") {

    assert((

      "\"\\r\"") == (

      "\"\\r\""))

  }

  test("showString - backslash") {

    assert((

      "\"\\\\\"") == (

      "\"\\\\\""))

  }

  test("showString - double quote") {

    assert((

      "\"\\\"\"") == (

      "\"\\\"\""))

  }

  test("showString - null") {

    assert((

      "\"\\NUL\"") == (

      "\"\\NUL\""))

  }

  test("showString - bell") {

    assert((

      "\"\\a\"") == (

      "\"\\a\""))

  }

  test("showString - backspace") {

    assert((

      "\"\\b\"") == (

      "\"\\b\""))

  }

  test("showString - form feed") {

    assert((

      "\"\\f\"") == (

      "\"\\f\""))

  }

  test("showString - vertical tab") {

    assert((

      "\"\\v\"") == (

      "\"\\v\""))

  }

  test("showString - delete") {

    assert((

      "\"\\DEL\"") == (

      "\"\\DEL\""))

  }

  // readInt8

  test("readInt8 - positive") {

    assert((

      just(42:int8)) == (

      just(42:int8)))

  }

  test("readInt8 - negative") {

    assert((

      just(-42:int8)) == (

      just(-42:int8)))

  }

  test("readInt8 - max value") {

    assert((

      just(127:int8)) == (

      just(127:int8)))

  }

  test("readInt8 - min value") {

    assert((

      just(-128:int8)) == (

      just(-128:int8)))

  }

  test("readInt8 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  test("readInt8 - overflow") {

    assert((

      nothing) == (

      nothing))

  }

  // readInt16

  test("readInt16 - positive") {

    assert((

      just(1000:int16)) == (

      just(1000:int16)))

  }

  test("readInt16 - negative") {

    assert((

      just(-1000:int16)) == (

      just(-1000:int16)))

  }

  test("readInt16 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readInt32

  test("readInt32 - positive") {

    assert((

      just(42:int32)) == (

      just(42:int32)))

  }

  test("readInt32 - negative") {

    assert((

      just(-42:int32)) == (

      just(-42:int32)))

  }

  test("readInt32 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readInt64

  test("readInt64 - positive") {

    assert((

      just(1000000:int64)) == (

      just(1000000:int64)))

  }

  test("readInt64 - negative") {

    assert((

      just(-1000000:int64)) == (

      just(-1000000:int64)))

  }

  test("readInt64 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readUint8

  test("readUint8 - zero") {

    assert((

      just(0:uint8)) == (

      just(0:uint8)))

  }

  test("readUint8 - typical") {

    assert((

      just(100:uint8)) == (

      just(100:uint8)))

  }

  test("readUint8 - max value") {

    assert((

      just(255:uint8)) == (

      just(255:uint8)))

  }

  test("readUint8 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  test("readUint8 - negative") {

    assert((

      nothing) == (

      nothing))

  }

  // readUint16

  test("readUint16 - zero") {

    assert((

      just(0:uint16)) == (

      just(0:uint16)))

  }

  test("readUint16 - typical") {

    assert((

      just(1000:uint16)) == (

      just(1000:uint16)))

  }

  test("readUint16 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  test("readUint16 - negative") {

    assert((

      nothing) == (

      nothing))

  }

  // readUint32

  test("readUint32 - zero") {

    assert((

      just(0:uint32)) == (

      just(0:uint32)))

  }

  test("readUint32 - typical") {

    assert((

      just(100000:uint32)) == (

      just(100000:uint32)))

  }

  test("readUint32 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  test("readUint32 - negative") {

    assert((

      nothing) == (

      nothing))

  }

  // readUint64

  test("readUint64 - zero") {

    assert((

      just(0:uint64)) == (

      just(0:uint64)))

  }

  test("readUint64 - typical") {

    assert((

      just(1000000:uint64)) == (

      just(1000000:uint64)))

  }

  test("readUint64 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  test("readUint64 - negative") {

    assert((

      nothing) == (

      nothing))

  }

  // readBigint

  test("readBigint - positive") {

    assert((

      just(42:bigint)) == (

      just(42:bigint)))

  }

  test("readBigint - negative") {

    assert((

      just(-42:bigint)) == (

      just(-42:bigint)))

  }

  test("readBigint - zero") {

    assert((

      just(0:bigint)) == (

      just(0:bigint)))

  }

  test("readBigint - large") {

    assert((

      just(123456789012345678901234567890:bigint)) == (

      just(123456789012345678901234567890:bigint)))

  }

  test("readBigint - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readFloat32

  test("readFloat32 - positive") {

    assert((

      just(3.14:float32)) == (

      just(3.14:float32)))

  }

  test("readFloat32 - negative") {

    assert((

      just(-2.5:float32)) == (

      just(-2.5:float32)))

  }

  test("readFloat32 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readFloat64

  test("readFloat64 - positive") {

    assert((

      just(3.14159:float64)) == (

      just(3.14159:float64)))

  }

  test("readFloat64 - negative") {

    assert((

      just(-2.71828:float64)) == (

      just(-2.71828:float64)))

  }

  test("readFloat64 - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readBigfloat

  test("readBigfloat - positive") {

    assert((

      just(3.14:bigfloat)) == (

      just(3.14:bigfloat)))

  }

  test("readBigfloat - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readBoolean

  test("readBoolean - true") {

    assert((

      just(true)) == (

      just(true)))

  }

  test("readBoolean - false") {

    assert((

      just(false)) == (

      just(false)))

  }

  test("readBoolean - invalid") {

    assert((

      nothing) == (

      nothing))

  }

  // readString

  test("readString - quoted string") {

    assert((

      just("hello")) == (

      just("hello")))

  }

  test("readString - empty quoted") {

    assert((

      just("")) == (

      just("")))

  }

  test("readString - unquoted") {

    assert((

      nothing) == (

      nothing))

  }

  // stringToBinary

  test("stringToBinary - simple base64") {

    assert((

      [binary]) == (

      [binary]))

  }

  test("stringToBinary - empty string") {

    assert((

      [binary]) == (

      [binary]))

  }

  // binaryToString

  test("binaryToString - simple binary") {

    assert((

      "aGVsbG8=") == (

      "aGVsbG8="))

  }

  test("binaryToString - empty binary") {

    assert((

      "") == (

      ""))

  }
}
