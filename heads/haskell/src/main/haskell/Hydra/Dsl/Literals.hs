-- | A DSL for constructing Hydra literal values in Haskell

module Hydra.Dsl.Literals where

import Hydra.Core

import Data.Int
import qualified Data.ByteString as B


-- | Create an arbitrary-precision floating point literal
-- Example: bigfloat 3.14159265359
bigfloat :: Double -> Literal
bigfloat = float . FloatValueBigfloat

-- | Create an arbitrary-precision integer literal
-- Example: bigint 9223372036854775808
bigint :: Integer -> Literal
bigint = integer . IntegerValueBigint . fromIntegral

-- | Create a binary data literal
-- Example: binary (B.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F])
binary :: B.ByteString -> Literal
binary = LiteralBinary

-- | Create a boolean literal
-- Example: boolean True
boolean :: Bool -> Literal
boolean = LiteralBoolean

-- | Create a 32-bit floating point literal
-- Example: float32 3.14
float32 :: Float -> Literal
float32 = float . FloatValueFloat32

-- | Create a 64-bit floating point literal
-- Example: float64 3.14159265359
float64 :: Double -> Literal
float64 = float . FloatValueFloat64

-- | Create a floating-point literal with specified precision
-- Example: float (FloatValueFloat32 3.14)
float :: FloatValue -> Literal
float = LiteralFloat

-- | Create a 16-bit signed integer literal
-- Example: int16 32767
int16 :: Int16 -> Literal
int16 = integer . IntegerValueInt16 . fromIntegral

-- | Create a 32-bit signed integer literal
-- Example: int32 42
int32 :: Int -> Literal
int32 = integer . IntegerValueInt32

-- | Create a 64-bit signed integer literal
-- Example: int64 9223372036854775807
int64 :: Int64 -> Literal
int64 = integer . IntegerValueInt64 . fromIntegral

-- | Create an 8-bit signed integer literal
-- Example: int8 127
int8 :: Int8 -> Literal
int8 = integer . IntegerValueInt8 . fromIntegral

-- | Create an integer literal with specified bit width
-- Example: integer (IntegerValueInt32 42)
integer :: IntegerValue -> Literal
integer = LiteralInteger

-- | Create a string literal
-- Example: string "hello world"
string :: String -> Literal
string = LiteralString

-- | Create a 16-bit unsigned integer literal
-- Example: uint16 65535
uint16 :: Int -> Literal
uint16 = integer . IntegerValueUint16 . fromIntegral

-- | Create a 32-bit unsigned integer literal
-- Example: uint32 4294967295
uint32 :: Int64 -> Literal
uint32 = integer . IntegerValueUint32 . fromIntegral

-- | Create a 64-bit unsigned integer literal
-- Example: uint64 18446744073709551615
uint64 :: Integer -> Literal
uint64 = integer . IntegerValueUint64 . fromIntegral

-- | Create an 8-bit unsigned integer literal
-- Example: uint8 255
uint8 :: Int16 -> Literal
uint8 = integer . IntegerValueUint8 . fromIntegral
