module Hydra.Impl.Haskell.Dsl.Literals where

import Hydra.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Data.Int


-- Note: does not yet properly capture arbitrary-precision floating-point numbers,
--       because code generation does not.
type Bigfloat = Double

-- Note: does not distinguish Binary from String, because code generation does not.
type Binary = String

bigfloat :: Bigfloat -> Datum Bigfloat
bigfloat = Datum . Terms.bigfloat

bigint :: Integer -> Datum Integer
bigint = Datum . Terms.bigint

binary :: Binary -> Datum Binary
binary = Datum . Terms.binaryTerm

bool :: Bool -> Datum Bool
bool = Datum . Terms.boolean

boolean :: Bool -> Datum Bool
boolean = bool

double :: Double -> Datum Double
double = float64

false :: Datum Bool
false = bool False

float :: Float -> Datum Float
float = float32

float32 :: Float -> Datum Float
float32 = Datum . Terms.float32

float64 :: Double -> Datum Double
float64 = Datum . Terms.float64

int :: Int -> Datum Int
int = int32

int8 :: Int8 -> Datum Int8
int8 = Datum . Terms.int8

int16 :: Int16 -> Datum Int16
int16 = Datum . Terms.int16

int32 :: Int -> Datum Int
int32 = Datum . Terms.int32

int64 :: Int64 -> Datum Int64
int64 = Datum . Terms.int64

string :: String -> Datum String
string = Datum . Terms.string

true :: Datum Bool
true = bool True

-- Note: untyped integers are not yet properly supported by the DSL,
--       because they are not properly supported by code generation.
uint8 :: Int8 -> Datum Int8
uint8 = int8
uint16 :: Int16 -> Datum Int16
uint16 = int16
uint32 :: Int -> Datum Int
uint32 = int
uint64 :: Int64 -> Datum Int64
uint64 = int64
