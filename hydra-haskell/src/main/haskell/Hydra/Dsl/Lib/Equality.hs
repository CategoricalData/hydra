module Hydra.Dsl.Lib.Equality where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Base

import Data.Int


equal :: TTerm a -> TTerm a -> TTerm Bool
equal = primitive2 _equality_equal

equalBinary :: TTerm String -> TTerm String -> TTerm Bool
equalBinary = primitive2 _equality_equalBinary

equalBoolean :: TTerm Bool -> TTerm Bool -> TTerm Bool
equalBoolean = primitive2 _equality_equalBoolean

equalBigfloat :: TTerm Double -> TTerm Double -> TTerm Bool
equalBigfloat = primitive2 _equality_equalBigfloat

equalFloat32 :: TTerm Float -> TTerm Float -> TTerm Bool
equalFloat32 = primitive2 _equality_equalFloat32

equalFloat64 :: TTerm Double -> TTerm Double -> TTerm Bool
equalFloat64 = primitive2 _equality_equalFloat64

equalBigint :: TTerm Integer -> TTerm Integer -> TTerm Bool
equalBigint = primitive2 _equality_equalBigint

equalInt8 :: TTerm Int8 -> TTerm Int8 -> TTerm Bool
equalInt8 = primitive2 _equality_equalInt8

equalInt16 :: TTerm Int16 -> TTerm Int16 -> TTerm Bool
equalInt16 = primitive2 _equality_equalInt16

equalInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
equalInt32 = primitive2 _equality_equalInt32

equalInt64 :: TTerm Int64 -> TTerm Int64 -> TTerm Bool
equalInt64 = primitive2 _equality_equalInt64

equalString :: TTerm String -> TTerm String -> TTerm Bool
equalString = primitive2 _equality_equalString

equalTerm :: TTerm Term -> TTerm Term -> TTerm Bool
equalTerm = primitive2 _equality_equalTerm

equalType :: TTerm Type -> TTerm Type -> TTerm Bool
equalType = primitive2 _equality_equalType

equalUint8 :: TTerm Int16 -> TTerm Int16 -> TTerm Bool
equalUint8 = primitive2 _equality_equalUint8

equalUint16 :: TTerm Int -> TTerm Int -> TTerm Bool
equalUint16 = primitive2 _equality_equalUint16

equalUint32 :: TTerm Int64 -> TTerm Int64 -> TTerm Bool
equalUint32 = primitive2 _equality_equalUint32

equalUint64 :: TTerm Integer -> TTerm Integer -> TTerm Bool
equalUint64 = primitive2 _equality_equalUint64

identity :: TTerm x -> TTerm x
identity = primitive1 _equality_identity

gtInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
gtInt32 = primitive2 _equality_gtInt32

gteInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
gteInt32 = primitive2 _equality_gteInt32

ltInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
ltInt32 = primitive2 _equality_ltInt32

lteInt32 :: TTerm Int -> TTerm Int -> TTerm Bool
lteInt32 = primitive2 _equality_lteInt32
