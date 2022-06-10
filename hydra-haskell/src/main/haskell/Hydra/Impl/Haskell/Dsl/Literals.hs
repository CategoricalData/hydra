module Hydra.Impl.Haskell.Dsl.Literals where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Data.Int


-- Note: does not yet properly capture arbitrary-precision floating-point numbers,
--       because code generation does not.
type Bigfloat = Double

-- Note: does not distinguish Binary from String, because code generation does not.
type Binary = String

bigfloat :: Bigfloat -> Trm Bigfloat
bigfloat = Trm . Terms.bigfloatValue

bigint :: Integer -> Trm Integer
bigint = Trm . Terms.bigintValue

binary :: Binary -> Trm Binary
binary = Trm . Terms.binaryTerm

bool :: Bool -> Trm Bool
bool = Trm . Terms.booleanValue

boolean :: Bool -> Trm Bool
boolean = bool

double :: Double -> Trm Double
double = float64

float :: Float -> Trm Float
float = float32

float32 :: Float -> Trm Float
float32 = Trm . Terms.float32Value

float64 :: Double -> Trm Double
float64 = Trm . Terms.float64Value

int :: Int -> Trm Int
int = int32

int8 :: Int8 -> Trm Int8
int8 = Trm . Terms.int8Value

int16 :: Int16 -> Trm Int16
int16 = Trm . Terms.int16Value

int32 :: Int -> Trm Int
int32 = Trm . Terms.int32Value

int64 :: Int64 -> Trm Int64
int64 = Trm . Terms.int64Value

str :: String -> Trm String
str = Trm . Terms.stringValue

string :: String -> Trm String
string = str

-- Note: untyped integers are not yet properly supported by the DSL,
--       because they are not properly supported by code generation.
uint8 :: Int8 -> Trm Int8
uint8 = int8
uint16 :: Int16 -> Trm Int16
uint16 = int16
uint32 :: Int -> Trm Int
uint32 = int
uint64 :: Int64 -> Trm Int64
uint64 = int64
