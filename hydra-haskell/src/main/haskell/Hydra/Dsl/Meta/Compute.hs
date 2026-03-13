{-# LANGUAGE FlexibleContexts #-}

-- | Meta-DSL for constructing compute-related terms (Adapter, Coder, etc.)

module Hydra.Dsl.Meta.Compute where

import Hydra.Kernel
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Meta.Core as Core

import qualified Data.Map as M
import qualified Data.Maybe as Y


adapter :: AsTerm c (Coder v1 v2) => TTerm Bool -> TTerm t1 -> TTerm t2 -> c -> TTerm (Adapter t1 t2 v1 v2)
adapter isLossy source target coderArg = record _Adapter [
  _Adapter_isLossy>>: isLossy,
  _Adapter_source>>: source,
  _Adapter_target>>: target,
  _Adapter_coder>>: asTerm coderArg]

adapterIsLossy :: TTerm (Adapter t1 t2 v1 v2) -> TTerm Bool
adapterIsLossy a = project _Adapter _Adapter_isLossy @@ a

adapterSource :: TTerm (Adapter t1 t2 v1 v2) -> TTerm t1
adapterSource a = project _Adapter _Adapter_source @@ a

adapterTarget :: TTerm (Adapter t1 t2 v1 v2) -> TTerm t2
adapterTarget a = project _Adapter _Adapter_target @@ a

adapterCoder :: TTerm (Adapter t1 t2 v1 v2) -> TTerm (Coder v1 v2)
adapterCoder a = project _Adapter _Adapter_coder @@ a

adapterWithCoder :: TTerm (Adapter t1 t2 v1 v2) -> TTerm (Coder v1 v2) -> TTerm (Adapter t1 t2 v1 v2)
adapterWithCoder a coder = adapter
  (Hydra.Dsl.Meta.Compute.adapterIsLossy a)
  (Hydra.Dsl.Meta.Compute.adapterSource a)
  (Hydra.Dsl.Meta.Compute.adapterTarget a)
  coder

adapterWithTarget :: TTerm (Adapter t1 t2 v1 v2) -> TTerm t2 -> TTerm (Adapter t1 t2 v1 v2)
adapterWithTarget a target = adapter
  (Hydra.Dsl.Meta.Compute.adapterIsLossy a)
  (Hydra.Dsl.Meta.Compute.adapterSource a)
  target
  (Hydra.Dsl.Meta.Compute.adapterCoder a)

coder :: TTerm (Context -> v1 -> Either (InContext Error) v2) -> TTerm (Context -> v2 -> Either (InContext Error) v1) -> TTerm (Coder v1 v2)
coder encode decode = record _Coder [
  _Coder_encode>>: encode,
  _Coder_decode>>: decode]

coderEncode :: TTerm (Coder v1 v2) -> TTerm (Context -> v1 -> Either (InContext Error) v2)
coderEncode c = project _Coder _Coder_encode @@ c

coderDecode :: TTerm (Coder v1 v2) -> TTerm (Context -> v2 -> Either (InContext Error) v1)
coderDecode c = project _Coder _Coder_decode @@ c

