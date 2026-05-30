-- | Phantom-typed term DSL for the hydra.lib.literals library

module Hydra.Dsl.Meta.Lib.Literals where

import Hydra.Typed
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import qualified Data.ByteString as B
import qualified Data.Scientific as Sci
import Data.Int


-- | Convert a bigint (Integer) to a decimal (Scientific).
bigintToDecimal :: TypedTerm Integer -> TypedTerm Sci.Scientific
bigintToDecimal = primitive1 _literals_bigintToDecimal

-- | Convert a bigint (Integer) to an int16.
bigintToInt16 :: TypedTerm Integer -> TypedTerm Int16
bigintToInt16 = primitive1 _literals_bigintToInt16

-- | Convert a bigint (Integer) to an int32.
bigintToInt32 :: TypedTerm Integer -> TypedTerm Int
bigintToInt32 = primitive1 _literals_bigintToInt32

-- | Convert a bigint (Integer) to an int64.
bigintToInt64 :: TypedTerm Integer -> TypedTerm Int64
bigintToInt64 = primitive1 _literals_bigintToInt64

-- | Convert a bigint (Integer) to an int8.
bigintToInt8 :: TypedTerm Integer -> TypedTerm Int8
bigintToInt8 = primitive1 _literals_bigintToInt8

-- | Convert a bigint (Integer) to a uint16.
bigintToUint16 :: TypedTerm Integer -> TypedTerm Int
bigintToUint16 = primitive1 _literals_bigintToUint16

-- | Convert a bigint (Integer) to a uint32.
bigintToUint32 :: TypedTerm Integer -> TypedTerm Int64
bigintToUint32 = primitive1 _literals_bigintToUint32

-- | Convert a bigint (Integer) to a uint64.
bigintToUint64 :: TypedTerm Integer -> TypedTerm Integer
bigintToUint64 = primitive1 _literals_bigintToUint64

-- | Convert a bigint (Integer) to a uint8.
bigintToUint8 :: TypedTerm Integer -> TypedTerm Int16
bigintToUint8 = primitive1 _literals_bigintToUint8

-- | Convert binary to a list of byte values (0-255).
binaryToBytes :: TypedTerm B.ByteString -> TypedTerm [Int]
binaryToBytes = primitive1 _literals_binaryToBytes

-- | Convert binary to string by base64 encoding.
binaryToString :: TypedTerm B.ByteString -> TypedTerm String
binaryToString = primitive1 _literals_binaryToString

-- | Convert a decimal (Scientific) to a bigint (Integer) by truncating toward zero.
decimalToBigint :: TypedTerm Sci.Scientific -> TypedTerm Integer
decimalToBigint = primitive1 _literals_decimalToBigint

-- | Convert a decimal (Scientific) to a float32 (Float). May lose precision.
decimalToFloat32 :: TypedTerm Sci.Scientific -> TypedTerm Float
decimalToFloat32 = primitive1 _literals_decimalToFloat32

-- | Convert a decimal (Scientific) to a float64 (Double). May lose precision.
decimalToFloat64 :: TypedTerm Sci.Scientific -> TypedTerm Double
decimalToFloat64 = primitive1 _literals_decimalToFloat64

-- | Convert a float32 (Float) to a decimal (Scientific).
float32ToDecimal :: TypedTerm Float -> TypedTerm Sci.Scientific
float32ToDecimal = primitive1 _literals_float32ToDecimal

-- | Convert a float32 (Float) to a float64 (Double).
float32ToFloat64 :: TypedTerm Float -> TypedTerm Double
float32ToFloat64 = primitive1 _literals_float32ToFloat64

-- | Convert a float64 (Double) to a decimal (Scientific).
float64ToDecimal :: TypedTerm Double -> TypedTerm Sci.Scientific
float64ToDecimal = primitive1 _literals_float64ToDecimal

-- | Convert a float64 (Double) to a float32 (Float). May lose precision.
float64ToFloat32 :: TypedTerm Double -> TypedTerm Float
float64ToFloat32 = primitive1 _literals_float64ToFloat32

-- | Convert an int16 to a bigint (Integer).
int16ToBigint :: TypedTerm Int16 -> TypedTerm Integer
int16ToBigint = primitive1 _literals_int16ToBigint

-- | Convert an int32 to a bigint (Integer).
int32ToBigint :: TypedTerm Int -> TypedTerm Integer
int32ToBigint = primitive1 _literals_int32ToBigint

-- | Convert an int64 to a bigint (Integer).
int64ToBigint :: TypedTerm Int64 -> TypedTerm Integer
int64ToBigint = primitive1 _literals_int64ToBigint

-- | Convert an int8 to a bigint (Integer).
int8ToBigint :: TypedTerm Int8 -> TypedTerm Integer
int8ToBigint = primitive1 _literals_int8ToBigint

-- | Parse a string to a bigint (Integer).
readBigint :: TypedTerm String -> TypedTerm (Maybe Integer)
readBigint = primitive1 _literals_readBigint

-- | Parse a string to a boolean.
readBoolean :: TypedTerm String -> TypedTerm (Maybe Bool)
readBoolean = primitive1 _literals_readBoolean

-- | Parse a string to a decimal (Scientific).
readDecimal :: TypedTerm String -> TypedTerm (Maybe Sci.Scientific)
readDecimal = primitive1 _literals_readDecimal

-- | Parse a string to a float32 (Float).
readFloat32 :: TypedTerm String -> TypedTerm (Maybe Float)
readFloat32 = primitive1 _literals_readFloat32

-- | Parse a string to a float64 (Double).
readFloat64 :: TypedTerm String -> TypedTerm (Maybe Double)
readFloat64 = primitive1 _literals_readFloat64

-- | Parse a string to an int16 (-32768 to 32767).
readInt16 :: TypedTerm String -> TypedTerm (Maybe Int16)
readInt16 = primitive1 _literals_readInt16

-- | Parse a string to an int32.
readInt32 :: TypedTerm String -> TypedTerm (Maybe Int)
readInt32 = primitive1 _literals_readInt32

-- | Parse a string to an int64.
readInt64 :: TypedTerm String -> TypedTerm (Maybe Int64)
readInt64 = primitive1 _literals_readInt64

-- | Parse a string to an int8 (-128 to 127).
readInt8 :: TypedTerm String -> TypedTerm (Maybe Int8)
readInt8 = primitive1 _literals_readInt8

-- | Parse a string literal.
readString :: TypedTerm String -> TypedTerm (Maybe String)
readString = primitive1 _literals_readString

-- | Parse a string to a uint16 (0 to 65535).
readUint16 :: TypedTerm String -> TypedTerm (Maybe Int)
readUint16 = primitive1 _literals_readUint16

-- | Parse a string to a uint32 (0 to 4294967295).
readUint32 :: TypedTerm String -> TypedTerm (Maybe Int64)
readUint32 = primitive1 _literals_readUint32

-- | Parse a string to a uint64 (0 to 18446744073709551615).
readUint64 :: TypedTerm String -> TypedTerm (Maybe Integer)
readUint64 = primitive1 _literals_readUint64

-- | Parse a string to a uint8 (0 to 255).
readUint8 :: TypedTerm String -> TypedTerm (Maybe Int16)
readUint8 = primitive1 _literals_readUint8

-- | Convert a bigint (Integer) to string.
showBigint :: TypedTerm Integer -> TypedTerm String
showBigint = primitive1 _literals_showBigint

-- | Convert a boolean to string.
showBoolean :: TypedTerm Bool -> TypedTerm String
showBoolean = primitive1 _literals_showBoolean

-- | Convert a decimal (Scientific) to string.
showDecimal :: TypedTerm Sci.Scientific -> TypedTerm String
showDecimal = primitive1 _literals_showDecimal

-- | Convert a float32 (Float) to string.
showFloat32 :: TypedTerm Float -> TypedTerm String
showFloat32 = primitive1 _literals_showFloat32

-- | Convert a float64 (Double) to string.
showFloat64 :: TypedTerm Double -> TypedTerm String
showFloat64 = primitive1 _literals_showFloat64

-- | Convert an int16 to string.
showInt16 :: TypedTerm Int16 -> TypedTerm String
showInt16 = primitive1 _literals_showInt16

-- | Convert an int32 to string.
showInt32 :: TypedTerm Int -> TypedTerm String
showInt32 = primitive1 _literals_showInt32

-- | Convert an int64 to string.
showInt64 :: TypedTerm Int64 -> TypedTerm String
showInt64 = primitive1 _literals_showInt64

-- | Convert an int8 to string.
showInt8 :: TypedTerm Int8 -> TypedTerm String
showInt8 = primitive1 _literals_showInt8

-- | Convert a string to a quoted string representation.
showString :: TypedTerm String -> TypedTerm String
showString = primitive1 _literals_showString

-- | Convert a uint16 to string.
showUint16 :: TypedTerm Int -> TypedTerm String
showUint16 = primitive1 _literals_showUint16

-- | Convert a uint32 to string.
showUint32 :: TypedTerm Int64 -> TypedTerm String
showUint32 = primitive1 _literals_showUint32

-- | Convert a uint64 to string.
showUint64 :: TypedTerm Integer -> TypedTerm String
showUint64 = primitive1 _literals_showUint64

-- | Convert a uint8 to string.
showUint8 :: TypedTerm Int16 -> TypedTerm String
showUint8 = primitive1 _literals_showUint8

-- | Convert string to binary by base64 decoding.
stringToBinary :: TypedTerm String -> TypedTerm B.ByteString
stringToBinary = primitive1 _literals_stringToBinary

-- | Convert a uint16 to a bigint (Integer).
uint16ToBigint :: TypedTerm Int -> TypedTerm Integer
uint16ToBigint = primitive1 _literals_uint16ToBigint

-- | Convert a uint32 to a bigint (Integer).
uint32ToBigint :: TypedTerm Int64 -> TypedTerm Integer
uint32ToBigint = primitive1 _literals_uint32ToBigint

-- | Convert a uint64 to a bigint (Integer).
uint64ToBigint :: TypedTerm Integer -> TypedTerm Integer
uint64ToBigint = primitive1 _literals_uint64ToBigint

-- | Convert a uint8 to a bigint (Integer).
uint8ToBigint :: TypedTerm Int16 -> TypedTerm Integer
uint8ToBigint = primitive1 _literals_uint8ToBigint
