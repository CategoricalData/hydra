module Hydra.Dsl.Lib.Literals where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms

import Data.Int


bigfloatToBigint :: TTerm (Double -> Double)
bigfloatToBigint = TTerm $ Terms.primitive _literals_bigfloatToBigint

bigfloatToFloat32 :: TTerm (Double -> Float)
bigfloatToFloat32 = TTerm $ Terms.primitive _literals_bigfloatToFloat32

bigfloatToFloat64 :: TTerm (Double -> Double)
bigfloatToFloat64 = TTerm $ Terms.primitive _literals_bigfloatToFloat64

bigintToBigfloat :: TTerm (Integer -> Double)
bigintToBigfloat = TTerm $ Terms.primitive _literals_bigintToBigfloat

bigintToInt8 :: TTerm (Integer -> Int8)
bigintToInt8 = TTerm $ Terms.primitive _literals_bigintToInt8

bigintToInt16 :: TTerm (Integer -> Int16)
bigintToInt16 = TTerm $ Terms.primitive _literals_bigintToInt16

bigintToInt32 :: TTerm (Integer -> Int)
bigintToInt32 = TTerm $ Terms.primitive _literals_bigintToInt32

bigintToInt64 :: TTerm (Integer -> Int64)
bigintToInt64 = TTerm $ Terms.primitive _literals_bigintToInt64

bigintToUint8 :: TTerm (Integer -> Int16)
bigintToUint8 = TTerm $ Terms.primitive _literals_bigintToUint8

bigintToUint16 :: TTerm (Integer -> Int)
bigintToUint16 = TTerm $ Terms.primitive _literals_bigintToUint16

bigintToUint32 :: TTerm (Integer -> Int64)
bigintToUint32 = TTerm $ Terms.primitive _literals_bigintToUint32

bigintToUint64 :: TTerm (Integer -> Integer)
bigintToUint64 = TTerm $ Terms.primitive _literals_bigintToUint64

float32ToBigfloat :: TTerm (Float -> Double)
float32ToBigfloat = TTerm $ Terms.primitive _literals_float32ToBigfloat

float64ToBigfloat :: TTerm (Double -> Double)
float64ToBigfloat = TTerm $ Terms.primitive _literals_float64ToBigfloat

int8ToBigint :: TTerm (Int8 -> Integer)
int8ToBigint = TTerm $ Terms.primitive _literals_int8ToBigint

int16ToBigint :: TTerm (Int16 -> Integer)
int16ToBigint = TTerm $ Terms.primitive _literals_int16ToBigint

int32ToBigint :: TTerm (Int -> Integer)
int32ToBigint = TTerm $ Terms.primitive _literals_int32ToBigint

int64ToBigint :: TTerm (Int64 -> Integer)
int64ToBigint = TTerm $ Terms.primitive _literals_int64ToBigint

readBoolean :: TTerm (String -> Maybe Bool)
readBoolean = TTerm $ Terms.primitive _literals_readBoolean

readInt32 :: TTerm (String -> Maybe Int)
readInt32 = TTerm $ Terms.primitive _literals_readInt32

readInt64 :: TTerm (String -> Maybe Int64)
readInt64 = TTerm $ Terms.primitive _literals_readInt64

readString :: TTerm (String -> Maybe String)
readString = TTerm $ Terms.primitive _literals_readString

showBoolean :: TTerm (Bool -> String)
showBoolean = TTerm $ Terms.primitive _literals_showBoolean

showInt32 :: TTerm (Int -> String)
showInt32 = TTerm $ Terms.primitive _literals_showInt32

showInt64 :: TTerm (Int64 -> String)
showInt64 = TTerm $ Terms.primitive _literals_showInt64

showString :: TTerm (String -> String)
showString = TTerm $ Terms.primitive _literals_showString

uint8ToBigint :: TTerm (Int16 -> Integer)
uint8ToBigint = TTerm $ Terms.primitive _literals_uint8ToBigint

uint16ToBigint :: TTerm (Int -> Integer)
uint16ToBigint = TTerm $ Terms.primitive _literals_uint16ToBigint

uint32ToBigint :: TTerm (Int64 -> Integer)
uint32ToBigint = TTerm $ Terms.primitive _literals_uint32ToBigint

uint64ToBigint :: TTerm (Integer -> Integer)
uint64ToBigint = TTerm $ Terms.primitive _literals_uint64ToBigint
