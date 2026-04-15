-- | Phantom-typed term DSL for the hydra.lib.literals library

module Hydra.Dsl.Meta.Lib.Literals where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import qualified Data.ByteString as B
import qualified Data.Scientific as Sci
import Data.Int


-- | Convert a bigfloat (Double) to a bigint (Integer).
bigfloatToBigint :: TTerm Double -> TTerm Integer
bigfloatToBigint = primitive1 _literals_bigfloatToBigint

-- | Convert a bigfloat (Double) to a float32 (Float).
bigfloatToFloat32 :: TTerm Double -> TTerm Float
bigfloatToFloat32 = primitive1 _literals_bigfloatToFloat32

-- | Convert a bigfloat (Double) to a float64 (Double).
bigfloatToFloat64 :: TTerm Double -> TTerm Double
bigfloatToFloat64 = primitive1 _literals_bigfloatToFloat64

-- | Convert a bigint (Integer) to a bigfloat (Double).
bigintToBigfloat :: TTerm Integer -> TTerm Double
bigintToBigfloat = primitive1 _literals_bigintToBigfloat

-- | Convert a bigint (Integer) to a decimal (Scientific).
bigintToDecimal :: TTerm Integer -> TTerm Sci.Scientific
bigintToDecimal = primitive1 _literals_bigintToDecimal

-- | Convert a bigint (Integer) to an int8.
bigintToInt8 :: TTerm Integer -> TTerm Int8
bigintToInt8 = primitive1 _literals_bigintToInt8

-- | Convert a bigint (Integer) to an int16.
bigintToInt16 :: TTerm Integer -> TTerm Int16
bigintToInt16 = primitive1 _literals_bigintToInt16

-- | Convert a bigint (Integer) to an int32.
bigintToInt32 :: TTerm Integer -> TTerm Int
bigintToInt32 = primitive1 _literals_bigintToInt32

-- | Convert a bigint (Integer) to an int64.
bigintToInt64 :: TTerm Integer -> TTerm Int64
bigintToInt64 = primitive1 _literals_bigintToInt64

-- | Convert a bigint (Integer) to a uint8.
bigintToUint8 :: TTerm Integer -> TTerm Int16
bigintToUint8 = primitive1 _literals_bigintToUint8

-- | Convert a bigint (Integer) to a uint16.
bigintToUint16 :: TTerm Integer -> TTerm Int
bigintToUint16 = primitive1 _literals_bigintToUint16

-- | Convert a bigint (Integer) to a uint32.
bigintToUint32 :: TTerm Integer -> TTerm Int64
bigintToUint32 = primitive1 _literals_bigintToUint32

-- | Convert a bigint (Integer) to a uint64.
bigintToUint64 :: TTerm Integer -> TTerm Integer
bigintToUint64 = primitive1 _literals_bigintToUint64

-- | Convert binary to a list of byte values (0-255).
binaryToBytes :: TTerm B.ByteString -> TTerm [Int]
binaryToBytes = primitive1 _literals_binaryToBytes

-- | Convert binary to string by base64 encoding.
binaryToString :: TTerm B.ByteString -> TTerm String
binaryToString = primitive1 _literals_binaryToString

-- | Convert a decimal (Scientific) to a bigint (Integer) by truncating toward zero.
decimalToBigint :: TTerm Sci.Scientific -> TTerm Integer
decimalToBigint = primitive1 _literals_decimalToBigint

-- | Convert a decimal (Scientific) to a float32 (Float). May lose precision.
decimalToFloat32 :: TTerm Sci.Scientific -> TTerm Float
decimalToFloat32 = primitive1 _literals_decimalToFloat32

-- | Convert a decimal (Scientific) to a float64 (Double). May lose precision.
decimalToFloat64 :: TTerm Sci.Scientific -> TTerm Double
decimalToFloat64 = primitive1 _literals_decimalToFloat64

-- | Convert a float32 (Float) to a bigfloat (Double).
float32ToBigfloat :: TTerm Float -> TTerm Double
float32ToBigfloat = primitive1 _literals_float32ToBigfloat

-- | Convert a float32 (Float) to a decimal (Scientific).
float32ToDecimal :: TTerm Float -> TTerm Sci.Scientific
float32ToDecimal = primitive1 _literals_float32ToDecimal

-- | Convert a float64 (Double) to a bigfloat (Double).
float64ToBigfloat :: TTerm Double -> TTerm Double
float64ToBigfloat = primitive1 _literals_float64ToBigfloat

-- | Convert a float64 (Double) to a decimal (Scientific).
float64ToDecimal :: TTerm Double -> TTerm Sci.Scientific
float64ToDecimal = primitive1 _literals_float64ToDecimal

-- | Convert an int8 to a bigint (Integer).
int8ToBigint :: TTerm Int8 -> TTerm Integer
int8ToBigint = primitive1 _literals_int8ToBigint

-- | Convert an int16 to a bigint (Integer).
int16ToBigint :: TTerm Int16 -> TTerm Integer
int16ToBigint = primitive1 _literals_int16ToBigint

-- | Convert an int32 to a bigint (Integer).
int32ToBigint :: TTerm Int -> TTerm Integer
int32ToBigint = primitive1 _literals_int32ToBigint

-- | Convert an int64 to a bigint (Integer).
int64ToBigint :: TTerm Int64 -> TTerm Integer
int64ToBigint = primitive1 _literals_int64ToBigint

-- | Parse a string to a bigfloat (Double).
readBigfloat :: TTerm String -> TTerm (Maybe Double)
readBigfloat = primitive1 _literals_readBigfloat

-- | Parse a string to a bigint (Integer).
readBigint :: TTerm String -> TTerm (Maybe Integer)
readBigint = primitive1 _literals_readBigint

-- | Parse a string to a boolean.
readBoolean :: TTerm String -> TTerm (Maybe Bool)
readBoolean = primitive1 _literals_readBoolean

-- | Parse a string to a decimal (Scientific).
readDecimal :: TTerm String -> TTerm (Maybe Sci.Scientific)
readDecimal = primitive1 _literals_readDecimal

-- | Parse a string to a float32 (Float).
readFloat32 :: TTerm String -> TTerm (Maybe Float)
readFloat32 = primitive1 _literals_readFloat32

-- | Parse a string to a float64 (Double).
readFloat64 :: TTerm String -> TTerm (Maybe Double)
readFloat64 = primitive1 _literals_readFloat64

-- | Parse a string to an int8 (-128 to 127).
readInt8 :: TTerm String -> TTerm (Maybe Int8)
readInt8 = primitive1 _literals_readInt8

-- | Parse a string to an int16 (-32768 to 32767).
readInt16 :: TTerm String -> TTerm (Maybe Int16)
readInt16 = primitive1 _literals_readInt16

-- | Parse a string to an int32.
readInt32 :: TTerm String -> TTerm (Maybe Int)
readInt32 = primitive1 _literals_readInt32

-- | Parse a string to an int64.
readInt64 :: TTerm String -> TTerm (Maybe Int64)
readInt64 = primitive1 _literals_readInt64

-- | Parse a string literal.
readString :: TTerm String -> TTerm (Maybe String)
readString = primitive1 _literals_readString

-- | Parse a string to a uint8 (0 to 255).
readUint8 :: TTerm String -> TTerm (Maybe Int16)
readUint8 = primitive1 _literals_readUint8

-- | Parse a string to a uint16 (0 to 65535).
readUint16 :: TTerm String -> TTerm (Maybe Int)
readUint16 = primitive1 _literals_readUint16

-- | Parse a string to a uint32 (0 to 4294967295).
readUint32 :: TTerm String -> TTerm (Maybe Int64)
readUint32 = primitive1 _literals_readUint32

-- | Parse a string to a uint64 (0 to 18446744073709551615).
readUint64 :: TTerm String -> TTerm (Maybe Integer)
readUint64 = primitive1 _literals_readUint64

-- | Convert a bigfloat (Double) to string.
showBigfloat :: TTerm Double -> TTerm String
showBigfloat = primitive1 _literals_showBigfloat

-- | Convert a bigint (Integer) to string.
showBigint :: TTerm Integer -> TTerm String
showBigint = primitive1 _literals_showBigint

-- | Convert a boolean to string.
showBoolean :: TTerm Bool -> TTerm String
showBoolean = primitive1 _literals_showBoolean

-- | Convert a decimal (Scientific) to string.
showDecimal :: TTerm Sci.Scientific -> TTerm String
showDecimal = primitive1 _literals_showDecimal

-- | Convert a float32 (Float) to string.
showFloat32 :: TTerm Float -> TTerm String
showFloat32 = primitive1 _literals_showFloat32

-- | Convert a float64 (Double) to string.
showFloat64 :: TTerm Double -> TTerm String
showFloat64 = primitive1 _literals_showFloat64

-- | Convert an int8 to string.
showInt8 :: TTerm Int8 -> TTerm String
showInt8 = primitive1 _literals_showInt8

-- | Convert an int16 to string.
showInt16 :: TTerm Int16 -> TTerm String
showInt16 = primitive1 _literals_showInt16

-- | Convert an int32 to string.
showInt32 :: TTerm Int -> TTerm String
showInt32 = primitive1 _literals_showInt32

-- | Convert an int64 to string.
showInt64 :: TTerm Int64 -> TTerm String
showInt64 = primitive1 _literals_showInt64

-- | Convert a string to a quoted string representation.
showString :: TTerm String -> TTerm String
showString = primitive1 _literals_showString

-- | Convert a uint8 to string.
showUint8 :: TTerm Int16 -> TTerm String
showUint8 = primitive1 _literals_showUint8

-- | Convert a uint16 to string.
showUint16 :: TTerm Int -> TTerm String
showUint16 = primitive1 _literals_showUint16

-- | Convert a uint32 to string.
showUint32 :: TTerm Int64 -> TTerm String
showUint32 = primitive1 _literals_showUint32

-- | Convert a uint64 to string.
showUint64 :: TTerm Integer -> TTerm String
showUint64 = primitive1 _literals_showUint64

-- | Convert string to binary by base64 decoding.
stringToBinary :: TTerm String -> TTerm B.ByteString
stringToBinary = primitive1 _literals_stringToBinary

-- | Convert a uint8 to a bigint (Integer).
uint8ToBigint :: TTerm Int16 -> TTerm Integer
uint8ToBigint = primitive1 _literals_uint8ToBigint

-- | Convert a uint16 to a bigint (Integer).
uint16ToBigint :: TTerm Int -> TTerm Integer
uint16ToBigint = primitive1 _literals_uint16ToBigint

-- | Convert a uint32 to a bigint (Integer).
uint32ToBigint :: TTerm Int64 -> TTerm Integer
uint32ToBigint = primitive1 _literals_uint32ToBigint

-- | Convert a uint64 to a bigint (Integer).
uint64ToBigint :: TTerm Integer -> TTerm Integer
uint64ToBigint = primitive1 _literals_uint64ToBigint
