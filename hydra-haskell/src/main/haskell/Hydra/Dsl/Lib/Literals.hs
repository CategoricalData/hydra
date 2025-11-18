-- | Phantom-typed term DSL for the hydra.lib.literals library

module Hydra.Dsl.Lib.Literals where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import Data.Int


bigfloatToBigint :: TTerm Double -> TTerm Double
bigfloatToBigint = primitive1 _literals_bigfloatToBigint

bigfloatToFloat32 :: TTerm Double -> TTerm Float
bigfloatToFloat32 = primitive1 _literals_bigfloatToFloat32

bigfloatToFloat64 :: TTerm Double -> TTerm Double
bigfloatToFloat64 = primitive1 _literals_bigfloatToFloat64

bigintToBigfloat :: TTerm Integer -> TTerm Double
bigintToBigfloat = primitive1 _literals_bigintToBigfloat

bigintToInt8 :: TTerm Integer -> TTerm Int8
bigintToInt8 = primitive1 _literals_bigintToInt8

bigintToInt16 :: TTerm Integer -> TTerm Int16
bigintToInt16 = primitive1 _literals_bigintToInt16

bigintToInt32 :: TTerm Integer -> TTerm Int
bigintToInt32 = primitive1 _literals_bigintToInt32

bigintToInt64 :: TTerm Integer -> TTerm Int64
bigintToInt64 = primitive1 _literals_bigintToInt64

bigintToUint8 :: TTerm Integer -> TTerm Int16
bigintToUint8 = primitive1 _literals_bigintToUint8

bigintToUint16 :: TTerm Integer -> TTerm Int
bigintToUint16 = primitive1 _literals_bigintToUint16

bigintToUint32 :: TTerm Integer -> TTerm Int64
bigintToUint32 = primitive1 _literals_bigintToUint32

bigintToUint64 :: TTerm Integer -> TTerm Integer
bigintToUint64 = primitive1 _literals_bigintToUint64

binaryToString :: TTerm String -> TTerm String
binaryToString = primitive1 _literals_binaryToString

float32ToBigfloat :: TTerm Float -> TTerm Double
float32ToBigfloat = primitive1 _literals_float32ToBigfloat

float64ToBigfloat :: TTerm Double -> TTerm Double
float64ToBigfloat = primitive1 _literals_float64ToBigfloat

int8ToBigint :: TTerm Int8 -> TTerm Integer
int8ToBigint = primitive1 _literals_int8ToBigint

int16ToBigint :: TTerm Int16 -> TTerm Integer
int16ToBigint = primitive1 _literals_int16ToBigint

int32ToBigint :: TTerm Int -> TTerm Integer
int32ToBigint = primitive1 _literals_int32ToBigint

int64ToBigint :: TTerm Int64 -> TTerm Integer
int64ToBigint = primitive1 _literals_int64ToBigint

readBigfloat :: TTerm String -> TTerm (Maybe Double)
readBigfloat = primitive1 _literals_readBigfloat

readBoolean :: TTerm String -> TTerm (Maybe Bool)
readBoolean = primitive1 _literals_readBoolean

readFloat32 :: TTerm String -> TTerm (Maybe Float)
readFloat32 = primitive1 _literals_readFloat32

readFloat64 :: TTerm String -> TTerm (Maybe Double)
readFloat64 = primitive1 _literals_readFloat64

readInt32 :: TTerm String -> TTerm (Maybe Int)
readInt32 = primitive1 _literals_readInt32

readInt64 :: TTerm String -> TTerm (Maybe Int64)
readInt64 = primitive1 _literals_readInt64

readString :: TTerm String -> TTerm (Maybe String)
readString = primitive1 _literals_readString

showBigfloat :: TTerm Double -> TTerm String
showBigfloat = primitive1 _literals_showBigfloat

showBigint :: TTerm Integer -> TTerm String
showBigint = primitive1 _literals_showBigint

showBoolean :: TTerm Bool -> TTerm String
showBoolean = primitive1 _literals_showBoolean

showFloat32 :: TTerm Float -> TTerm String
showFloat32 = primitive1 _literals_showFloat32

showFloat64 :: TTerm Double -> TTerm String
showFloat64 = primitive1 _literals_showFloat64

showInt8 :: TTerm Int8 -> TTerm String
showInt8 = primitive1 _literals_showInt8

showInt16 :: TTerm Int16 -> TTerm String
showInt16 = primitive1 _literals_showInt16

showInt32 :: TTerm Int -> TTerm String
showInt32 = primitive1 _literals_showInt32

showInt64 :: TTerm Int64 -> TTerm String
showInt64 = primitive1 _literals_showInt64

showUint8 :: TTerm Int16 -> TTerm String
showUint8 = primitive1 _literals_showUint8

showUint16 :: TTerm Int -> TTerm String
showUint16 = primitive1 _literals_showUint16

showUint32 :: TTerm Int64 -> TTerm String
showUint32 = primitive1 _literals_showUint32

showUint64 :: TTerm Integer -> TTerm String
showUint64 = primitive1 _literals_showUint64

showString :: TTerm String -> TTerm String
showString = primitive1 _literals_showString

stringToBinary :: TTerm String -> TTerm String
stringToBinary = primitive1 _literals_stringToBinary

uint8ToBigint :: TTerm Int16 -> TTerm Integer
uint8ToBigint = primitive1 _literals_uint8ToBigint

uint16ToBigint :: TTerm Int -> TTerm Integer
uint16ToBigint = primitive1 _literals_uint16ToBigint

uint32ToBigint :: TTerm Int64 -> TTerm Integer
uint32ToBigint = primitive1 _literals_uint32ToBigint

uint64ToBigint :: TTerm Integer -> TTerm Integer
uint64ToBigint = primitive1 _literals_uint64ToBigint
