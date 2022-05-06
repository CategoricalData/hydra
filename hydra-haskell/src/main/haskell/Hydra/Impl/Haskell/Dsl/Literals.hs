module Hydra.Impl.Haskell.Dsl.Literals where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Phantoms
import Data.Int


-- Note: does not yet properly capture arbitrary-precision floating-point numbers,
--       because code generation does not.
type Bigfloat = Double

-- Note: does not distinguish Binary from String, because code generation does not.
type Binary = String

bigfloat :: Bigfloat -> Program Bigfloat
bigfloat f = program $ DataTermLiteral $ LiteralFloat $ FloatValueBigfloat f

bigint :: Integer -> Program Integer
bigint i = program $ DataTermLiteral $ LiteralInteger $ IntegerValueBigint i

binary :: Binary -> Program Binary
binary b = program $ DataTermLiteral $ LiteralBinary b

bool :: Bool -> Program Bool
bool b = program $ DataTermLiteral $ LiteralBoolean $ if b then BooleanValueTrue else BooleanValueFalse

boolean = bool

double = float64

float = float32

float32 :: Float -> Program Float
float32 f = program $ DataTermLiteral $ LiteralFloat $ FloatValueFloat32 f

float64 :: Double -> Program Double
float64 f = program $ DataTermLiteral $ LiteralFloat $ FloatValueFloat64 f

int = int32

int8 :: Int8 -> Program Int8
int8 i = program $ DataTermLiteral $ LiteralInteger $ IntegerValueInt8 $ fromIntegral i

int16 :: Int16 -> Program Int16
int16 i = program $ DataTermLiteral $ LiteralInteger $ IntegerValueInt16 $ fromIntegral i

int32 :: Int -> Program Int
int32 i = program $ DataTermLiteral $ LiteralInteger $ IntegerValueInt32 i

int64 :: Int64 -> Program Int64
int64 i = program $ DataTermLiteral $ LiteralInteger $ IntegerValueInt64 $ fromIntegral i

str :: String -> Program String
str s = program $ DataTermLiteral $ LiteralString s

-- Note: untyped integers are not yet properly supported by the DSL,
--       because they are not properly supported by code generation.
uint8 = int8
uint16 = int16
uint32 = int
uint64 = int64
