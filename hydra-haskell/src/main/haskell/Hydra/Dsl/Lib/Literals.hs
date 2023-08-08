module Hydra.Dsl.Lib.Literals where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms

import Data.Int


bigfloatToBigint :: Datum (Double -> Double)
bigfloatToBigint = Datum $ Terms.primitive _literals_bigfloatToBigint

bigfloatToFloat32 :: Datum (Double -> Float)
bigfloatToFloat32 = Datum $ Terms.primitive _literals_bigfloatToFloat32

bigfloatToFloat64 :: Datum (Double -> Double)
bigfloatToFloat64 = Datum $ Terms.primitive _literals_bigfloatToFloat64

bigintToBigfloat :: Datum (Integer -> Double)
bigintToBigfloat = Datum $ Terms.primitive _literals_bigintToBigfloat

bigintToInt8 :: Datum (Integer -> Int8)
bigintToInt8 = Datum $ Terms.primitive _literals_bigintToInt8

bigintToInt16 :: Datum (Integer -> Int16)
bigintToInt16 = Datum $ Terms.primitive _literals_bigintToInt16

bigintToInt32 :: Datum (Integer -> Int)
bigintToInt32 = Datum $ Terms.primitive _literals_bigintToInt32

bigintToInt64 :: Datum (Integer -> Int64)
bigintToInt64 = Datum $ Terms.primitive _literals_bigintToInt64

bigintToUint8 :: Datum (Integer -> Int16)
bigintToUint8 = Datum $ Terms.primitive _literals_bigintToUint8

bigintToUint16 :: Datum (Integer -> Int)
bigintToUint16 = Datum $ Terms.primitive _literals_bigintToUint16

bigintToUint32 :: Datum (Integer -> Int64)
bigintToUint32 = Datum $ Terms.primitive _literals_bigintToUint32

bigintToUint64 :: Datum (Integer -> Integer)
bigintToUint64 = Datum $ Terms.primitive _literals_bigintToUint64

float32ToBigfloat :: Datum (Float -> Double)
float32ToBigfloat = Datum $ Terms.primitive _literals_float32ToBigfloat

float64ToBigfloat :: Datum (Double -> Double)
float64ToBigfloat = Datum $ Terms.primitive _literals_float64ToBigfloat

int8ToBigint :: Datum (Int8 -> Integer)
int8ToBigint = Datum $ Terms.primitive _literals_int8ToBigint

int16ToBigint :: Datum (Int16 -> Integer)
int16ToBigint = Datum $ Terms.primitive _literals_int16ToBigint

int32ToBigint :: Datum (Int -> Integer)
int32ToBigint = Datum $ Terms.primitive _literals_int32ToBigint

int64ToBigint :: Datum (Int64 -> Integer)
int64ToBigint = Datum $ Terms.primitive _literals_int64ToBigint

showInt32 :: Datum (Int -> String)
showInt32 = Datum $ Terms.primitive _literals_showInt32

showString :: Datum (String -> String)
showString = Datum $ Terms.primitive _literals_showString

uint8ToBigint :: Datum (Int16 -> Integer)
uint8ToBigint = Datum $ Terms.primitive _literals_uint8ToBigint

uint16ToBigint :: Datum (Int -> Integer)
uint16ToBigint = Datum $ Terms.primitive _literals_uint16ToBigint

uint32ToBigint :: Datum (Int64 -> Integer)
uint32ToBigint = Datum $ Terms.primitive _literals_uint32ToBigint

uint64ToBigint :: Datum (Integer -> Integer)
uint64ToBigint = Datum $ Terms.primitive _literals_uint64ToBigint
