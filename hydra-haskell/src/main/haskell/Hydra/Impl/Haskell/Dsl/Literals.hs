module Hydra.Impl.Haskell.Dsl.Literals where

import Hydra.Core
import Hydra.Graph
import Hydra.Monads

import Data.Int


bigfloat :: Double -> Literal
bigfloat = float . FloatValueBigfloat

bigint :: Integer -> Literal
bigint = integer . IntegerValueBigint . fromIntegral

binary :: String -> Literal
binary = LiteralBinary

boolean :: Bool -> Literal
boolean = LiteralBoolean

expectBoolean :: Show m => Literal -> GraphFlow m Bool
expectBoolean v = case v of
  LiteralBoolean b -> pure b
  _ -> unexpected "boolean" v

expectInt32 :: Show m => Literal -> GraphFlow m Int
expectInt32 v = case v of
  LiteralInteger (IntegerValueInt32 i) -> pure i
  _ -> unexpected "int32" v

expectString :: Show m => Literal -> GraphFlow m String
expectString v = case v of
  LiteralString s -> pure s
  _ -> unexpected "string" v

float32 :: Float -> Literal
float32 = float . FloatValueFloat32

float64 :: Double -> Literal
float64 = float . FloatValueFloat64

float :: FloatValue -> Literal
float = LiteralFloat

int16 :: Int16 -> Literal
int16 = integer . IntegerValueInt16 . fromIntegral

int32 :: Int -> Literal
int32 = integer . IntegerValueInt32

int64 :: Int64 -> Literal
int64 = integer . IntegerValueInt64 . fromIntegral

int8 :: Int8 -> Literal
int8 = integer . IntegerValueInt8 . fromIntegral

integer :: IntegerValue -> Literal
integer = LiteralInteger

string :: String -> Literal
string = LiteralString

uint16 :: Integer -> Literal
uint16 = integer . IntegerValueUint16 . fromIntegral

uint32 :: Integer -> Literal
uint32 = integer . IntegerValueUint32 . fromIntegral

uint64 :: Integer -> Literal
uint64 = integer . IntegerValueUint64 . fromIntegral

uint8 :: Integer -> Literal
uint8 = integer . IntegerValueUint8 . fromIntegral
