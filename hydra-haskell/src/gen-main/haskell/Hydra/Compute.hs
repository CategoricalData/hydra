-- Note: this is an automatically generated file. Do not edit.

-- | Abstractions for single- and bidirectional transformations

module Hydra.Compute where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A two-level bidirectional encoder which adapts types to types and terms to terms
data Adapter t1 t2 v1 v2 = 
  Adapter {
    -- | Whether information may be lost in the course of this adaptation
    adapterIsLossy :: Bool,
    -- | The source type
    adapterSource :: t1,
    -- | The target type
    adapterTarget :: t2,
    -- | The coder for transforming instances of the source type to instances of the target type
    adapterCoder :: (Coder v1 v2)}

_Adapter = (Core.Name "hydra.compute.Adapter")

_Adapter_isLossy = (Core.Name "isLossy")

_Adapter_source = (Core.Name "source")

_Adapter_target = (Core.Name "target")

_Adapter_coder = (Core.Name "coder")

-- | A two-level encoder and decoder, operating both at a type level and an instance (data) level
data Bicoder t1 t2 v1 v2 = 
  Bicoder {
    -- | A function from source types to adapters
    bicoderEncode :: (t1 -> Adapter t1 t2 v1 v2),
    -- | A function from target types to adapters
    bicoderDecode :: (t2 -> Adapter t2 t1 v2 v1)}

_Bicoder = (Core.Name "hydra.compute.Bicoder")

_Bicoder_encode = (Core.Name "encode")

_Bicoder_decode = (Core.Name "decode")

-- | An encoder and decoder; a bidirectional transformation between two types
data Coder v1 v2 = 
  Coder {
    -- | A function which encodes source values as target values in a given context
    coderEncode :: (Context.Context -> v1 -> Either (Context.InContext Error.OtherError) v2),
    -- | A function which decodes target values as source values in a given context
    coderDecode :: (Context.Context -> v2 -> Either (Context.InContext Error.OtherError) v1)}

_Coder = (Core.Name "hydra.compute.Coder")

_Coder_encode = (Core.Name "encode")

_Coder_decode = (Core.Name "decode")
