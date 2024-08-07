module Hydra.Dsl.Lib.Equality where

import Hydra.Core
import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms

import Data.Int


equal :: TTerm (a -> a -> Bool)
equal = TTerm $ Terms.primitive _equality_equal

equalBinary :: TTerm (String -> String -> Bool)
equalBinary = TTerm $ Terms.primitive _equality_equalBinary

equalBoolean :: TTerm (Bool -> Bool -> Bool)
equalBoolean = TTerm $ Terms.primitive _equality_equalBoolean

equalBigfloat :: TTerm (Double -> Double -> Bool)
equalBigfloat = TTerm $ Terms.primitive _equality_equalBigfloat

equalFloat32 :: TTerm (Float -> Float -> Bool)
equalFloat32 = TTerm $ Terms.primitive _equality_equalFloat32

equalFloat64 :: TTerm (Double -> Double -> Bool)
equalFloat64 = TTerm $ Terms.primitive _equality_equalFloat64

equalBigint :: TTerm (Integer -> Integer -> Bool)
equalBigint = TTerm $ Terms.primitive _equality_equalBigint

equalInt8 :: TTerm (Int8 -> Int8 -> Bool)
equalInt8 = TTerm $ Terms.primitive _equality_equalInt8

equalInt16 :: TTerm (Int16 -> Int16 -> Bool)
equalInt16 = TTerm $ Terms.primitive _equality_equalInt16

equalInt32 :: TTerm (Int -> Int -> Bool)
equalInt32 = TTerm $ Terms.primitive _equality_equalInt32

equalInt64 :: TTerm (Int64 -> Int64 -> Bool)
equalInt64 = TTerm $ Terms.primitive _equality_equalInt64

equalString :: TTerm (String -> String -> Bool)
equalString = TTerm $ Terms.primitive _equality_equalString

equalTerm :: TTerm (Term -> Term -> Bool)
equalTerm = TTerm $ Terms.primitive _equality_equalTerm

equalType :: TTerm (Type -> Type -> Bool)
equalType = TTerm $ Terms.primitive _equality_equalType

equalUint8 :: TTerm (Int16 -> Int16 -> Bool)
equalUint8 = TTerm $ Terms.primitive _equality_equalUint8

equalUint16 :: TTerm (Int -> Int -> Bool)
equalUint16 = TTerm $ Terms.primitive _equality_equalUint16

equalUint32 :: TTerm (Int64 -> Int64 -> Bool)
equalUint32 = TTerm $ Terms.primitive _equality_equalUint32

equalUint64 :: TTerm (Integer -> Integer -> Bool)
equalUint64 = TTerm $ Terms.primitive _equality_equalUint64

identity :: TTerm (x -> x)
identity = TTerm $ Terms.primitive _equality_identity

gtInt32 :: TTerm (Int -> Int -> Bool)
gtInt32 = TTerm $ Terms.primitive _equality_gtInt32

gteInt32 :: TTerm (Int -> Int -> Bool)
gteInt32 = TTerm $ Terms.primitive _equality_gteInt32

ltInt32 :: TTerm (Int -> Int -> Bool)
ltInt32 = TTerm $ Terms.primitive _equality_ltInt32

lteInt32 :: TTerm (Int -> Int -> Bool)
lteInt32 = TTerm $ Terms.primitive _equality_lteInt32
