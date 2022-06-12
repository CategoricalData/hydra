module Hydra.Impl.Haskell.Dsl.Literals where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Data.Int


-- Note: does not yet properly capture arbitrary-precision floating-point numbers,
--       because code generation does not.
type Bigfloat = Double

-- Note: does not distinguish Binary from String, because code generation does not.
type Binary = String

bigfloat :: Bigfloat -> Data Bigfloat
bigfloat = Data . Terms.bigfloat

bigint :: Integer -> Data Integer
bigint = Data . Terms.bigint

binary :: Binary -> Data Binary
binary = Data . Terms.binaryTerm

bool :: Bool -> Data Bool
bool = Data . Terms.boolean

boolean :: Bool -> Data Bool
boolean = bool

double :: Double -> Data Double
double = float64

false :: Data Bool
false = bool False

float :: Float -> Data Float
float = float32

float32 :: Float -> Data Float
float32 = Data . Terms.float32

float64 :: Double -> Data Double
float64 = Data . Terms.float64

int :: Int -> Data Int
int = int32

int8 :: Int8 -> Data Int8
int8 = Data . Terms.int8

int16 :: Int16 -> Data Int16
int16 = Data . Terms.int16

int32 :: Int -> Data Int
int32 = Data . Terms.int32

int64 :: Int64 -> Data Int64
int64 = Data . Terms.int64

string :: String -> Data String
string = Data . Terms.string

true :: Data Bool
true = bool True

-- Note: untyped integers are not yet properly supported by the DSL,
--       because they are not properly supported by code generation.
uint8 :: Int8 -> Data Int8
uint8 = int8
uint16 :: Int16 -> Data Int16
uint16 = int16
uint32 :: Int -> Data Int
uint32 = int
uint64 :: Int64 -> Data Int64
uint64 = int64
