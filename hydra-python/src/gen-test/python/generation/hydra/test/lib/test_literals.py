# Note: this is an automatically generated file. Do not edit.
# hydra.lib.literals primitives

from __future__ import annotations
from typing import cast
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing
import hydra.accessors
import hydra.annotations
import hydra.ast
import hydra.classes
import hydra.coders
import hydra.compute
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.ext.haskell.operators
import hydra.extract.core
import hydra.extract.helpers
import hydra.formatting
import hydra.graph
import hydra.json
import hydra.lexical
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.parsing
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.tarjan
import hydra.testing
import hydra.topology
import hydra.typing
import hydra.util

# bigintToInt8

def test_biginttoint8__positive():

    assert (hydra.lib.literals.bigint_to_int8(42)) == (42)

def test_biginttoint8__negative():

    assert (hydra.lib.literals.bigint_to_int8(-42)) == (-42)

# bigintToInt16

def test_biginttoint16__positive():

    assert (hydra.lib.literals.bigint_to_int16(1000)) == (1000)

def test_biginttoint16__negative():

    assert (hydra.lib.literals.bigint_to_int16(-1000)) == (-1000)

# bigintToInt32

def test_biginttoint32__positive():

    assert (hydra.lib.literals.bigint_to_int32(42)) == (42)

def test_biginttoint32__negative():

    assert (hydra.lib.literals.bigint_to_int32(-42)) == (-42)

def test_biginttoint32__zero():

    assert (hydra.lib.literals.bigint_to_int32(0)) == (0)

# bigintToInt64

def test_biginttoint64__positive():

    assert (hydra.lib.literals.bigint_to_int64(1000000)) == (1000000)

def test_biginttoint64__negative():

    assert (hydra.lib.literals.bigint_to_int64(-1000000)) == (-1000000)

# bigintToUint8

def test_biginttouint8__zero():

    assert (hydra.lib.literals.bigint_to_uint8(0)) == (0)

def test_biginttouint8__typical_value():

    assert (hydra.lib.literals.bigint_to_uint8(100)) == (100)

# bigintToUint16

def test_biginttouint16__zero():

    assert (hydra.lib.literals.bigint_to_uint16(0)) == (0)

def test_biginttouint16__typical_value():

    assert (hydra.lib.literals.bigint_to_uint16(1000)) == (1000)

# bigintToUint32

def test_biginttouint32__zero():

    assert (hydra.lib.literals.bigint_to_uint32(0)) == (0)

def test_biginttouint32__typical_value():

    assert (hydra.lib.literals.bigint_to_uint32(100000)) == (100000)

# bigintToUint64

def test_biginttouint64__zero():

    assert (hydra.lib.literals.bigint_to_uint64(0)) == (0)

def test_biginttouint64__typical_value():

    assert (hydra.lib.literals.bigint_to_uint64(1000000)) == (1000000)

# int8ToBigint

def test_int8tobigint__positive():

    assert (hydra.lib.literals.int8_to_bigint(42)) == (42)

def test_int8tobigint__negative():

    assert (hydra.lib.literals.int8_to_bigint(-42)) == (-42)

def test_int8tobigint__max_value():

    assert (hydra.lib.literals.int8_to_bigint(127)) == (127)

def test_int8tobigint__min_value():

    assert (hydra.lib.literals.int8_to_bigint(-128)) == (-128)

# int16ToBigint

def test_int16tobigint__positive():

    assert (hydra.lib.literals.int16_to_bigint(1000)) == (1000)

def test_int16tobigint__negative():

    assert (hydra.lib.literals.int16_to_bigint(-1000)) == (-1000)

# int32ToBigint

def test_int32tobigint__positive():

    assert (hydra.lib.literals.int32_to_bigint(42)) == (42)

def test_int32tobigint__negative():

    assert (hydra.lib.literals.int32_to_bigint(-42)) == (-42)

def test_int32tobigint__zero():

    assert (hydra.lib.literals.int32_to_bigint(0)) == (0)

# int64ToBigint

def test_int64tobigint__positive():

    assert (hydra.lib.literals.int64_to_bigint(1000000)) == (1000000)

def test_int64tobigint__negative():

    assert (hydra.lib.literals.int64_to_bigint(-1000000)) == (-1000000)

# uint8ToBigint

def test_uint8tobigint__zero():

    assert (hydra.lib.literals.uint8_to_bigint(0)) == (0)

def test_uint8tobigint__max_value():

    assert (hydra.lib.literals.uint8_to_bigint(255)) == (255)

# uint16ToBigint

def test_uint16tobigint__zero():

    assert (hydra.lib.literals.uint16_to_bigint(0)) == (0)

def test_uint16tobigint__typical_value():

    assert (hydra.lib.literals.uint16_to_bigint(1000)) == (1000)

# uint32ToBigint

def test_uint32tobigint__zero():

    assert (hydra.lib.literals.uint32_to_bigint(0)) == (0)

def test_uint32tobigint__typical_value():

    assert (hydra.lib.literals.uint32_to_bigint(100000)) == (100000)

# uint64ToBigint

def test_uint64tobigint__zero():

    assert (hydra.lib.literals.uint64_to_bigint(0)) == (0)

def test_uint64tobigint__typical_value():

    assert (hydra.lib.literals.uint64_to_bigint(1000000)) == (1000000)

# float32ToBigfloat

def test_float32tobigfloat__positive():

    assert (hydra.lib.literals.float32_to_bigfloat(2.5)) == (Decimal('2.5'))

def test_float32tobigfloat__negative():

    assert (hydra.lib.literals.float32_to_bigfloat(-2.5)) == (Decimal('-2.5'))

def test_float32tobigfloat__zero():

    assert (hydra.lib.literals.float32_to_bigfloat(0.0)) == (Decimal('0.0'))

# float64ToBigfloat

def test_float64tobigfloat__positive():

    assert (hydra.lib.literals.float64_to_bigfloat(3.14159)) == (Decimal('3.14159'))

def test_float64tobigfloat__negative():

    assert (hydra.lib.literals.float64_to_bigfloat(-2.71828)) == (Decimal('-2.71828'))

def test_float64tobigfloat__zero():

    assert (hydra.lib.literals.float64_to_bigfloat(0.0)) == (Decimal('0.0'))

# bigfloatToFloat32

def test_bigfloattofloat32__positive():

    assert (hydra.lib.literals.bigfloat_to_float32(Decimal('3.14'))) == (3.140000104904175)

def test_bigfloattofloat32__negative():

    assert (hydra.lib.literals.bigfloat_to_float32(Decimal('-2.5'))) == (-2.5)

def test_bigfloattofloat32__zero():

    assert (hydra.lib.literals.bigfloat_to_float32(Decimal('0.0'))) == (0.0)

# bigfloatToFloat64

def test_bigfloattofloat64__positive():

    assert (hydra.lib.literals.bigfloat_to_float64(Decimal('3.14159'))) == (3.14159)

def test_bigfloattofloat64__negative():

    assert (hydra.lib.literals.bigfloat_to_float64(Decimal('-2.71828'))) == (-2.71828)

def test_bigfloattofloat64__zero():

    assert (hydra.lib.literals.bigfloat_to_float64(Decimal('0.0'))) == (0.0)

# bigintToBigfloat

def test_biginttobigfloat__positive():

    assert (hydra.lib.literals.bigint_to_bigfloat(42)) == (Decimal('42.0'))

def test_biginttobigfloat__negative():

    assert (hydra.lib.literals.bigint_to_bigfloat(-42)) == (Decimal('-42.0'))

def test_biginttobigfloat__zero():

    assert (hydra.lib.literals.bigint_to_bigfloat(0)) == (Decimal('0.0'))

# bigfloatToBigint

def test_bigfloattobigint__positive():

    assert (hydra.lib.literals.bigfloat_to_bigint(Decimal('42.7'))) == (43)

def test_bigfloattobigint__negative():

    assert (hydra.lib.literals.bigfloat_to_bigint(Decimal('-42.7'))) == (-43)

def test_bigfloattobigint__zero():

    assert (hydra.lib.literals.bigfloat_to_bigint(Decimal('0.0'))) == (0)

def test_bigfloattobigint__round_down():

    assert (hydra.lib.literals.bigfloat_to_bigint(Decimal('42.3'))) == (42)

def test_bigfloattobigint__half_even_up():

    assert (hydra.lib.literals.bigfloat_to_bigint(Decimal('42.5'))) == (42)

def test_bigfloattobigint__half_even_down():

    assert (hydra.lib.literals.bigfloat_to_bigint(Decimal('43.5'))) == (44)

# showInt8

def test_showint8__positive():

    assert (hydra.lib.literals.show_int8(42)) == ("42")

def test_showint8__negative():

    assert (hydra.lib.literals.show_int8(-42)) == ("-42")

# showInt16

def test_showint16__positive():

    assert (hydra.lib.literals.show_int16(1000)) == ("1000")

def test_showint16__negative():

    assert (hydra.lib.literals.show_int16(-1000)) == ("-1000")

# showInt32

def test_showint32__positive():

    assert (hydra.lib.literals.show_int32(42)) == ("42")

def test_showint32__negative():

    assert (hydra.lib.literals.show_int32(-42)) == ("-42")

def test_showint32__zero():

    assert (hydra.lib.literals.show_int32(0)) == ("0")

# showInt64

def test_showint64__positive():

    assert (hydra.lib.literals.show_int64(1000000)) == ("1000000")

def test_showint64__negative():

    assert (hydra.lib.literals.show_int64(-1000000)) == ("-1000000")

# showUint8

def test_showuint8__zero():

    assert (hydra.lib.literals.show_uint8(0)) == ("0")

def test_showuint8__max_value():

    assert (hydra.lib.literals.show_uint8(255)) == ("255")

# showUint16

def test_showuint16__zero():

    assert (hydra.lib.literals.show_uint16(0)) == ("0")

def test_showuint16__typical_value():

    assert (hydra.lib.literals.show_uint16(1000)) == ("1000")

# showUint32

def test_showuint32__zero():

    assert (hydra.lib.literals.show_uint32(0)) == ("0")

def test_showuint32__typical_value():

    assert (hydra.lib.literals.show_uint32(100000)) == ("100000")

# showUint64

def test_showuint64__zero():

    assert (hydra.lib.literals.show_uint64(0)) == ("0")

def test_showuint64__typical_value():

    assert (hydra.lib.literals.show_uint64(1000000)) == ("1000000")

# showBigint

def test_showbigint__positive():

    assert (hydra.lib.literals.show_bigint(42)) == ("42")

def test_showbigint__negative():

    assert (hydra.lib.literals.show_bigint(-42)) == ("-42")

def test_showbigint__zero():

    assert (hydra.lib.literals.show_bigint(0)) == ("0")

# showFloat32

def test_showfloat32__positive():

    assert (hydra.lib.literals.show_float32(3.140000104904175)) == ("3.14")

def test_showfloat32__negative():

    assert (hydra.lib.literals.show_float32(-2.5)) == ("-2.5")

def test_showfloat32__zero():

    assert (hydra.lib.literals.show_float32(0.0)) == ("0.0")

# showFloat64

def test_showfloat64__positive():

    assert (hydra.lib.literals.show_float64(3.14159)) == ("3.14159")

def test_showfloat64__zero():

    assert (hydra.lib.literals.show_float64(0.0)) == ("0.0")

# showBigfloat

def test_showbigfloat__positive():

    assert (hydra.lib.literals.show_bigfloat(Decimal('3.14'))) == ("3.14")

def test_showbigfloat__zero():

    assert (hydra.lib.literals.show_bigfloat(Decimal('0.0'))) == ("0.0")

# showBoolean

def test_showboolean__true():

    assert (hydra.lib.literals.show_boolean(True)) == ("true")

def test_showboolean__false():

    assert (hydra.lib.literals.show_boolean(False)) == ("false")

# showString

def test_showstring__simple():

    assert (hydra.lib.literals.show_string("hello")) == ("\"hello\"")

def test_showstring__empty():

    assert (hydra.lib.literals.show_string("")) == ("\"\"")

# readInt8

def test_readint8__positive():

    assert (hydra.lib.literals.read_int8("42")) == (Just(42))

def test_readint8__negative():

    assert (hydra.lib.literals.read_int8("-42")) == (Just(-42))

def test_readint8__max_value():

    assert (hydra.lib.literals.read_int8("127")) == (Just(127))

def test_readint8__min_value():

    assert (hydra.lib.literals.read_int8("-128")) == (Just(-128))

def test_readint8__invalid():

    assert (hydra.lib.literals.read_int8("abc")) == (Nothing())

def test_readint8__overflow():

    assert (hydra.lib.literals.read_int8("128")) == (Nothing())

# readInt16

def test_readint16__positive():

    assert (hydra.lib.literals.read_int16("1000")) == (Just(1000))

def test_readint16__negative():

    assert (hydra.lib.literals.read_int16("-1000")) == (Just(-1000))

def test_readint16__invalid():

    assert (hydra.lib.literals.read_int16("abc")) == (Nothing())

# readInt32

def test_readint32__positive():

    assert (hydra.lib.literals.read_int32("42")) == (Just(42))

def test_readint32__negative():

    assert (hydra.lib.literals.read_int32("-42")) == (Just(-42))

def test_readint32__invalid():

    assert (hydra.lib.literals.read_int32("abc")) == (Nothing())

# readInt64

def test_readint64__positive():

    assert (hydra.lib.literals.read_int64("1000000")) == (Just(1000000))

def test_readint64__negative():

    assert (hydra.lib.literals.read_int64("-1000000")) == (Just(-1000000))

def test_readint64__invalid():

    assert (hydra.lib.literals.read_int64("abc")) == (Nothing())

# readUint8

def test_readuint8__zero():

    assert (hydra.lib.literals.read_uint8("0")) == (Just(0))

def test_readuint8__typical():

    assert (hydra.lib.literals.read_uint8("100")) == (Just(100))

def test_readuint8__max_value():

    assert (hydra.lib.literals.read_uint8("255")) == (Just(255))

def test_readuint8__invalid():

    assert (hydra.lib.literals.read_uint8("abc")) == (Nothing())

def test_readuint8__negative():

    assert (hydra.lib.literals.read_uint8("-1")) == (Nothing())

# readUint16

def test_readuint16__zero():

    assert (hydra.lib.literals.read_uint16("0")) == (Just(0))

def test_readuint16__typical():

    assert (hydra.lib.literals.read_uint16("1000")) == (Just(1000))

def test_readuint16__invalid():

    assert (hydra.lib.literals.read_uint16("abc")) == (Nothing())

def test_readuint16__negative():

    assert (hydra.lib.literals.read_uint16("-1")) == (Nothing())

# readUint32

def test_readuint32__zero():

    assert (hydra.lib.literals.read_uint32("0")) == (Just(0))

def test_readuint32__typical():

    assert (hydra.lib.literals.read_uint32("100000")) == (Just(100000))

def test_readuint32__invalid():

    assert (hydra.lib.literals.read_uint32("abc")) == (Nothing())

def test_readuint32__negative():

    assert (hydra.lib.literals.read_uint32("-1")) == (Nothing())

# readUint64

def test_readuint64__zero():

    assert (hydra.lib.literals.read_uint64("0")) == (Just(0))

def test_readuint64__typical():

    assert (hydra.lib.literals.read_uint64("1000000")) == (Just(1000000))

def test_readuint64__invalid():

    assert (hydra.lib.literals.read_uint64("abc")) == (Nothing())

def test_readuint64__negative():

    assert (hydra.lib.literals.read_uint64("-1")) == (Nothing())

# readBigint

def test_readbigint__positive():

    assert (hydra.lib.literals.read_bigint("42")) == (Just(42))

def test_readbigint__negative():

    assert (hydra.lib.literals.read_bigint("-42")) == (Just(-42))

def test_readbigint__zero():

    assert (hydra.lib.literals.read_bigint("0")) == (Just(0))

def test_readbigint__large():

    assert (hydra.lib.literals.read_bigint("123456789012345678901234567890")) == (Just(123456789012345678901234567890))

def test_readbigint__invalid():

    assert (hydra.lib.literals.read_bigint("abc")) == (Nothing())

# readFloat32

def test_readfloat32__positive():

    assert (hydra.lib.literals.read_float32("3.14")) == (Just(3.140000104904175))

def test_readfloat32__negative():

    assert (hydra.lib.literals.read_float32("-2.5")) == (Just(-2.5))

def test_readfloat32__invalid():

    assert (hydra.lib.literals.read_float32("abc")) == (Nothing())

# readFloat64

def test_readfloat64__positive():

    assert (hydra.lib.literals.read_float64("3.14159")) == (Just(3.14159))

def test_readfloat64__negative():

    assert (hydra.lib.literals.read_float64("-2.71828")) == (Just(-2.71828))

def test_readfloat64__invalid():

    assert (hydra.lib.literals.read_float64("abc")) == (Nothing())

# readBigfloat

def test_readbigfloat__positive():

    assert (hydra.lib.literals.read_bigfloat("3.14")) == (Just(Decimal('3.14')))

def test_readbigfloat__invalid():

    assert (hydra.lib.literals.read_bigfloat("abc")) == (Nothing())

# readBoolean

def test_readboolean__true():

    assert (hydra.lib.literals.read_boolean("true")) == (Just(True))

def test_readboolean__false():

    assert (hydra.lib.literals.read_boolean("false")) == (Just(False))

def test_readboolean__invalid():

    assert (hydra.lib.literals.read_boolean("yes")) == (Nothing())

# readString

def test_readstring__quoted_string():

    assert (hydra.lib.literals.read_string("\"hello\"")) == (Just("hello"))

def test_readstring__empty_quoted():

    assert (hydra.lib.literals.read_string("\"\"")) == (Just(""))

def test_readstring__unquoted():

    assert (hydra.lib.literals.read_string("hello")) == (Nothing())

# stringToBinary

def test_stringtobinary__simple_base64():

    assert (hydra.lib.literals.string_to_binary("aGVsbG8=")) == (bytes([104, 101, 108, 108, 111]))

def test_stringtobinary__empty_string():

    assert (hydra.lib.literals.string_to_binary("")) == (bytes([]))

# binaryToString

def test_binarytostring__simple_binary():

    assert (hydra.lib.literals.binary_to_string(bytes([104, 101, 108, 108, 111]))) == ("aGVsbG8=")

def test_binarytostring__empty_binary():

    assert (hydra.lib.literals.binary_to_string(bytes([]))) == ("")
