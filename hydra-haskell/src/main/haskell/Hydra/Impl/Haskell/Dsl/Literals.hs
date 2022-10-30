module Hydra.Impl.Haskell.Dsl.Literals where

import Hydra.All

import Data.Int


bigfloat :: Double -> Literal
bigfloat = float . FloatValueBigfloat

bigint :: Integer -> Literal
bigint = integer . IntegerValueBigint . fromIntegral

binary :: String -> Literal
binary = LiteralBinary

boolean :: Bool -> Literal
boolean = LiteralBoolean

expectBinary :: Literal -> Flow s String
expectBinary v = case v of
  LiteralBinary b -> pure b
  _ -> unexpected "binary" v

expectBoolean :: Literal -> Flow s Bool
expectBoolean v = case v of
  LiteralBoolean b -> pure b
  _ -> unexpected "boolean" v

expectFloat32 :: Literal -> Flow s Float
expectFloat32 v = case v of
  LiteralFloat (FloatValueFloat32 f) -> pure f
  _ -> unexpected "float32" v

expectFloat64 :: Literal -> Flow s Double
expectFloat64 v = case v of
  LiteralFloat (FloatValueFloat64 f) -> pure f
  _ -> unexpected "float64" v

expectInt32 :: Literal -> Flow s Int
expectInt32 v = case v of
  LiteralInteger (IntegerValueInt32 i) -> pure i
  _ -> unexpected "int32" v

expectInt64 :: Literal -> Flow s Integer
expectInt64 v = case v of
  LiteralInteger (IntegerValueInt64 i) -> pure i
  _ -> unexpected "int64" v

expectString :: Literal -> Flow s String
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
