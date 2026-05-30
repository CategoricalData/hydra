-- | A DSL for constructing literal terms using Haskell's built-in datatypes

module Hydra.Dsl.Meta.Literals where

import Hydra.Typed
import qualified Hydra.Dsl.Terms as Terms

import Data.Int
import qualified Data.ByteString as B
import qualified Data.Scientific as Sci


-- Binary is now properly represented as ByteString
type Binary = B.ByteString

bigint :: Integer -> TypedTerm Integer
bigint = TypedTerm . Terms.bigint

binary :: Binary -> TypedTerm Binary
binary = TypedTerm . Terms.binary

bool :: Bool -> TypedTerm Bool
bool = TypedTerm . Terms.boolean

boolean :: Bool -> TypedTerm Bool
boolean = bool

char :: Char -> TypedTerm Int
char = TypedTerm . Terms.char

decimal :: Sci.Scientific -> TypedTerm Sci.Scientific
decimal = TypedTerm . Terms.decimal

double :: Double -> TypedTerm Double
double = float64

false :: TypedTerm Bool
false = bool False

float :: Float -> TypedTerm Float
float = float32

float32 :: Float -> TypedTerm Float
float32 = TypedTerm . Terms.float32

float64 :: Double -> TypedTerm Double
float64 = TypedTerm . Terms.float64

int :: Int -> TypedTerm Int
int = int32

int16 :: Int16 -> TypedTerm Int16
int16 = TypedTerm . Terms.int16

int32 :: Int -> TypedTerm Int
int32 = TypedTerm . Terms.int32

int64 :: Int64 -> TypedTerm Int64
int64 = TypedTerm . Terms.int64

int8 :: Int8 -> TypedTerm Int8
int8 = TypedTerm . Terms.int8

string :: String -> TypedTerm String
string = TypedTerm . Terms.string

true :: TypedTerm Bool
true = bool True

uint16 :: Int -> TypedTerm Int
uint16 = TypedTerm . Terms.uint16

uint32 :: Int64 -> TypedTerm Int64
uint32 = TypedTerm . Terms.uint32

uint64 :: Integer -> TypedTerm Integer
uint64 = TypedTerm . Terms.uint64

uint8 :: Int16 -> TypedTerm Int16
uint8 = TypedTerm . Terms.uint8
