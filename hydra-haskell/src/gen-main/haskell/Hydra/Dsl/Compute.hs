-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.compute

module Hydra.Dsl.Compute where

import qualified Hydra.Compute as Compute
import qualified Hydra.Context as Context
import qualified Hydra.Error as Error
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adapter :: (Bool -> t0 -> t1 -> Compute.Coder t2 t3 -> Compute.Adapter t0 t1 t2 t3)
adapter isLossy source target coder = Compute.Adapter {
  Compute.adapterIsLossy = isLossy,
  Compute.adapterSource = source,
  Compute.adapterTarget = target,
  Compute.adapterCoder = coder}

adapterIsLossy :: (Compute.Adapter t0 t1 t2 t3 -> Bool)
adapterIsLossy = Compute.adapterIsLossy

adapterSource :: (Compute.Adapter t0 t1 t2 t3 -> t0)
adapterSource = Compute.adapterSource

adapterTarget :: (Compute.Adapter t0 t1 t2 t3 -> t1)
adapterTarget = Compute.adapterTarget

adapterCoder :: (Compute.Adapter t0 t1 t2 t3 -> Compute.Coder t2 t3)
adapterCoder = Compute.adapterCoder

adapterWithIsLossy :: (Compute.Adapter t0 t1 t2 t3 -> Bool -> Compute.Adapter t0 t1 t2 t3)
adapterWithIsLossy original newVal = Compute.Adapter {
  Compute.adapterIsLossy = newVal,
  Compute.adapterSource = (Compute.adapterSource original),
  Compute.adapterTarget = (Compute.adapterTarget original),
  Compute.adapterCoder = (Compute.adapterCoder original)}

adapterWithSource :: (Compute.Adapter t0 t1 t2 t3 -> t4 -> Compute.Adapter t4 t1 t2 t3)
adapterWithSource original newVal = Compute.Adapter {
  Compute.adapterIsLossy = (Compute.adapterIsLossy original),
  Compute.adapterSource = newVal,
  Compute.adapterTarget = (Compute.adapterTarget original),
  Compute.adapterCoder = (Compute.adapterCoder original)}

adapterWithTarget :: (Compute.Adapter t0 t1 t2 t3 -> t4 -> Compute.Adapter t0 t4 t2 t3)
adapterWithTarget original newVal = Compute.Adapter {
  Compute.adapterIsLossy = (Compute.adapterIsLossy original),
  Compute.adapterSource = (Compute.adapterSource original),
  Compute.adapterTarget = newVal,
  Compute.adapterCoder = (Compute.adapterCoder original)}

adapterWithCoder :: (Compute.Adapter t0 t1 t2 t3 -> Compute.Coder t4 t5 -> Compute.Adapter t0 t1 t4 t5)
adapterWithCoder original newVal = Compute.Adapter {
  Compute.adapterIsLossy = (Compute.adapterIsLossy original),
  Compute.adapterSource = (Compute.adapterSource original),
  Compute.adapterTarget = (Compute.adapterTarget original),
  Compute.adapterCoder = newVal}

bicoder :: ((t0 -> Compute.Adapter t0 t1 t2 t3) -> (t1 -> Compute.Adapter t1 t0 t3 t2) -> Compute.Bicoder t0 t1 t2 t3)
bicoder encode decode = Compute.Bicoder {
  Compute.bicoderEncode = encode,
  Compute.bicoderDecode = decode}

bicoderEncode :: (Compute.Bicoder t0 t1 t2 t3 -> t0 -> Compute.Adapter t0 t1 t2 t3)
bicoderEncode = Compute.bicoderEncode

bicoderDecode :: (Compute.Bicoder t0 t1 t2 t3 -> t1 -> Compute.Adapter t1 t0 t3 t2)
bicoderDecode = Compute.bicoderDecode

bicoderWithEncode :: (Compute.Bicoder t0 t1 t2 t3 -> (t0 -> Compute.Adapter t0 t1 t2 t3) -> Compute.Bicoder t0 t1 t2 t3)
bicoderWithEncode original newVal = Compute.Bicoder {
  Compute.bicoderEncode = newVal,
  Compute.bicoderDecode = (Compute.bicoderDecode original)}

bicoderWithDecode :: (Compute.Bicoder t0 t1 t2 t3 -> (t1 -> Compute.Adapter t1 t0 t3 t2) -> Compute.Bicoder t0 t1 t2 t3)
bicoderWithDecode original newVal = Compute.Bicoder {
  Compute.bicoderEncode = (Compute.bicoderEncode original),
  Compute.bicoderDecode = newVal}

coder :: ((Context.Context -> t0 -> Either (Context.InContext Error.Error) t1) -> (Context.Context -> t1 -> Either (Context.InContext Error.Error) t0) -> Compute.Coder t0 t1)
coder encode decode = Compute.Coder {
  Compute.coderEncode = encode,
  Compute.coderDecode = decode}

coderEncode :: (Compute.Coder t0 t1 -> Context.Context -> t0 -> Either (Context.InContext Error.Error) t1)
coderEncode = Compute.coderEncode

coderDecode :: (Compute.Coder t0 t1 -> Context.Context -> t1 -> Either (Context.InContext Error.Error) t0)
coderDecode = Compute.coderDecode

coderWithEncode :: (Compute.Coder t0 t1 -> (Context.Context -> t0 -> Either (Context.InContext Error.Error) t1) -> Compute.Coder t0 t1)
coderWithEncode original newVal = Compute.Coder {
  Compute.coderEncode = newVal,
  Compute.coderDecode = (Compute.coderDecode original)}

coderWithDecode :: (Compute.Coder t0 t1 -> (Context.Context -> t1 -> Either (Context.InContext Error.Error) t0) -> Compute.Coder t0 t1)
coderWithDecode original newVal = Compute.Coder {
  Compute.coderEncode = (Compute.coderEncode original),
  Compute.coderDecode = newVal}
