-- | A DSL for Hydra literal types in Haskell

module Hydra.Dsl.LiteralTypes where

import Hydra.Core


-- | Arbitrary-precision floating point type
-- Example: bigfloat
bigfloat :: LiteralType
bigfloat = float FloatTypeBigfloat

-- | Arbitrary-precision integer type
-- Example: bigint
bigint :: LiteralType
bigint = LiteralTypeInteger IntegerTypeBigint

-- | Binary data type
-- Example: binary
binary :: LiteralType
binary = LiteralTypeBinary

-- | Boolean type
-- Example: boolean
boolean :: LiteralType
boolean = LiteralTypeBoolean

-- | 32-bit floating point type
-- Example: float32
float32 :: LiteralType
float32 = float FloatTypeFloat32

-- | 64-bit floating point type
-- Example: float64
float64 :: LiteralType
float64 = float FloatTypeFloat64

-- | Create a floating point type with the specified precision
-- Example: float FloatTypeFloat32
float :: FloatType -> LiteralType
float = LiteralTypeFloat

-- | 16-bit signed integer type
-- Example: int16
int16 :: LiteralType
int16 = integer IntegerTypeInt16

-- | 32-bit signed integer type
-- Example: int32
int32 :: LiteralType
int32 = integer IntegerTypeInt32

-- | 64-bit signed integer type
-- Example: int64
int64 :: LiteralType
int64 = integer IntegerTypeInt64

-- | 8-bit signed integer type
-- Example: int8
int8 :: LiteralType
int8 = integer IntegerTypeInt8

-- | Create an integer type with the specified bit width
-- Example: integer IntegerTypeInt32
integer :: IntegerType -> LiteralType
integer = LiteralTypeInteger

-- | String type
-- Example: string
string :: LiteralType
string = LiteralTypeString

-- | 16-bit unsigned integer type
-- Example: uint16
uint16 :: LiteralType
uint16 = integer IntegerTypeUint16

-- | 32-bit unsigned integer type
-- Example: uint32
uint32 :: LiteralType
uint32 = integer IntegerTypeUint32

-- | 64-bit unsigned integer type
-- Example: uint64
uint64 :: LiteralType
uint64 = integer IntegerTypeUint64

-- | 8-bit unsigned integer type
-- Example: uint8
uint8 :: LiteralType
uint8 = integer IntegerTypeUint8
