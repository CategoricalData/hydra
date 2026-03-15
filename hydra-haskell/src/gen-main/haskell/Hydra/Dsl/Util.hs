-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.util

module Hydra.Dsl.Util where

import qualified Hydra.Context as Context
import qualified Hydra.Error as Error
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adapter :: (Bool -> t0 -> t1 -> Util.Coder t2 t3 -> Util.Adapter t0 t1 t2 t3)
adapter isLossy source target coder = Util.Adapter {
  Util.adapterIsLossy = isLossy,
  Util.adapterSource = source,
  Util.adapterTarget = target,
  Util.adapterCoder = coder}

adapterIsLossy :: (Util.Adapter t0 t1 t2 t3 -> Bool)
adapterIsLossy = Util.adapterIsLossy

adapterSource :: (Util.Adapter t0 t1 t2 t3 -> t0)
adapterSource = Util.adapterSource

adapterTarget :: (Util.Adapter t0 t1 t2 t3 -> t1)
adapterTarget = Util.adapterTarget

adapterCoder :: (Util.Adapter t0 t1 t2 t3 -> Util.Coder t2 t3)
adapterCoder = Util.adapterCoder

adapterWithIsLossy :: (Util.Adapter t0 t1 t2 t3 -> Bool -> Util.Adapter t0 t1 t2 t3)
adapterWithIsLossy original newVal = Util.Adapter {
  Util.adapterIsLossy = newVal,
  Util.adapterSource = (Util.adapterSource original),
  Util.adapterTarget = (Util.adapterTarget original),
  Util.adapterCoder = (Util.adapterCoder original)}

adapterWithSource :: (Util.Adapter t0 t1 t2 t3 -> t4 -> Util.Adapter t4 t1 t2 t3)
adapterWithSource original newVal = Util.Adapter {
  Util.adapterIsLossy = (Util.adapterIsLossy original),
  Util.adapterSource = newVal,
  Util.adapterTarget = (Util.adapterTarget original),
  Util.adapterCoder = (Util.adapterCoder original)}

adapterWithTarget :: (Util.Adapter t0 t1 t2 t3 -> t4 -> Util.Adapter t0 t4 t2 t3)
adapterWithTarget original newVal = Util.Adapter {
  Util.adapterIsLossy = (Util.adapterIsLossy original),
  Util.adapterSource = (Util.adapterSource original),
  Util.adapterTarget = newVal,
  Util.adapterCoder = (Util.adapterCoder original)}

adapterWithCoder :: (Util.Adapter t0 t1 t2 t3 -> Util.Coder t4 t5 -> Util.Adapter t0 t1 t4 t5)
adapterWithCoder original newVal = Util.Adapter {
  Util.adapterIsLossy = (Util.adapterIsLossy original),
  Util.adapterSource = (Util.adapterSource original),
  Util.adapterTarget = (Util.adapterTarget original),
  Util.adapterCoder = newVal}

bicoder :: ((t0 -> Util.Adapter t0 t1 t2 t3) -> (t1 -> Util.Adapter t1 t0 t3 t2) -> Util.Bicoder t0 t1 t2 t3)
bicoder encode decode = Util.Bicoder {
  Util.bicoderEncode = encode,
  Util.bicoderDecode = decode}

bicoderEncode :: (Util.Bicoder t0 t1 t2 t3 -> t0 -> Util.Adapter t0 t1 t2 t3)
bicoderEncode = Util.bicoderEncode

bicoderDecode :: (Util.Bicoder t0 t1 t2 t3 -> t1 -> Util.Adapter t1 t0 t3 t2)
bicoderDecode = Util.bicoderDecode

bicoderWithEncode :: (Util.Bicoder t0 t1 t2 t3 -> (t0 -> Util.Adapter t0 t1 t2 t3) -> Util.Bicoder t0 t1 t2 t3)
bicoderWithEncode original newVal = Util.Bicoder {
  Util.bicoderEncode = newVal,
  Util.bicoderDecode = (Util.bicoderDecode original)}

bicoderWithDecode :: (Util.Bicoder t0 t1 t2 t3 -> (t1 -> Util.Adapter t1 t0 t3 t2) -> Util.Bicoder t0 t1 t2 t3)
bicoderWithDecode original newVal = Util.Bicoder {
  Util.bicoderEncode = (Util.bicoderEncode original),
  Util.bicoderDecode = newVal}

caseConventionCamel :: Util.CaseConvention
caseConventionCamel = Util.CaseConventionCamel

caseConventionPascal :: Util.CaseConvention
caseConventionPascal = Util.CaseConventionPascal

caseConventionLowerSnake :: Util.CaseConvention
caseConventionLowerSnake = Util.CaseConventionLowerSnake

caseConventionUpperSnake :: Util.CaseConvention
caseConventionUpperSnake = Util.CaseConventionUpperSnake

coder :: ((Context.Context -> t0 -> Either (Context.InContext Error.Error) t1) -> (Context.Context -> t1 -> Either (Context.InContext Error.Error) t0) -> Util.Coder t0 t1)
coder encode decode = Util.Coder {
  Util.coderEncode = encode,
  Util.coderDecode = decode}

coderEncode :: (Util.Coder t0 t1 -> Context.Context -> t0 -> Either (Context.InContext Error.Error) t1)
coderEncode = Util.coderEncode

coderDecode :: (Util.Coder t0 t1 -> Context.Context -> t1 -> Either (Context.InContext Error.Error) t0)
coderDecode = Util.coderDecode

coderWithEncode :: (Util.Coder t0 t1 -> (Context.Context -> t0 -> Either (Context.InContext Error.Error) t1) -> Util.Coder t0 t1)
coderWithEncode original newVal = Util.Coder {
  Util.coderEncode = newVal,
  Util.coderDecode = (Util.coderDecode original)}

coderWithDecode :: (Util.Coder t0 t1 -> (Context.Context -> t1 -> Either (Context.InContext Error.Error) t0) -> Util.Coder t0 t1)
coderWithDecode original newVal = Util.Coder {
  Util.coderEncode = (Util.coderEncode original),
  Util.coderDecode = newVal}

comparisonLessThan :: Util.Comparison
comparisonLessThan = Util.ComparisonLessThan

comparisonEqualTo :: Util.Comparison
comparisonEqualTo = Util.ComparisonEqualTo

comparisonGreaterThan :: Util.Comparison
comparisonGreaterThan = Util.ComparisonGreaterThan

precisionArbitrary :: Util.Precision
precisionArbitrary = Util.PrecisionArbitrary

precisionBits :: (Int -> Util.Precision)
precisionBits x = (Util.PrecisionBits x)
