-- | A DSL for constructing literal terms using Haskell's built-in datatypes

module Hydra.Dsl.Meta.Literals where

import Hydra.Phantoms
import qualified Hydra.Dsl.Terms as Terms

import Data.Int


-- Note: does not yet properly capture arbitrary-precision floating-point numbers,
--       because code generation does not.
type Bigfloat = Double

-- Note: does not distinguish Binary from String, because code generation does not.
type Binary = String

bigfloat :: Bigfloat -> TTerm Bigfloat
bigfloat = TTerm . Terms.bigfloat

bigint :: Integer -> TTerm Integer
bigint = TTerm . Terms.bigint

binary :: Binary -> TTerm Binary
binary = TTerm . Terms.binary

bool :: Bool -> TTerm Bool
bool = TTerm . Terms.boolean

boolean :: Bool -> TTerm Bool
boolean = bool

char :: Char -> TTerm Int
char = TTerm . Terms.char

double :: Double -> TTerm Double
double = float64

false :: TTerm Bool
false = bool False

float :: Float -> TTerm Float
float = float32

float32 :: Float -> TTerm Float
float32 = TTerm . Terms.float32

float64 :: Double -> TTerm Double
float64 = TTerm . Terms.float64

int :: Int -> TTerm Int
int = int32

int8 :: Int8 -> TTerm Int8
int8 = TTerm . Terms.int8

int16 :: Int16 -> TTerm Int16
int16 = TTerm . Terms.int16

int32 :: Int -> TTerm Int
int32 = TTerm . Terms.int32

int64 :: Int64 -> TTerm Int64
int64 = TTerm . Terms.int64

string :: String -> TTerm String
string = TTerm . Terms.string

true :: TTerm Bool
true = bool True

uint8 :: Int16 -> TTerm Int16
uint8 = TTerm . Terms.uint8

uint16 :: Int -> TTerm Int
uint16 = TTerm . Terms.uint16

uint32 :: Int64 -> TTerm Int64
uint32 = TTerm . Terms.uint32

uint64 :: Integer -> TTerm Integer
uint64 = TTerm . Terms.uint64
