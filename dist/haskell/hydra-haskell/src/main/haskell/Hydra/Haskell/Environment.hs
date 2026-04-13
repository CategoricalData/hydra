-- Note: this is an automatically generated file. Do not edit.

-- | Environment types for Haskell code generation

module Hydra.Haskell.Environment where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Metadata used to determine which standard imports are needed in a generated Haskell module
data HaskellModuleMetadata =
  HaskellModuleMetadata {
    -- | Whether the module uses Data.ByteString (B.ByteString)
    haskellModuleMetadataUsesByteString :: Bool,
    -- | Whether the module uses Data.Int (I.Int8, I.Int16, I.Int64)
    haskellModuleMetadataUsesInt :: Bool,
    -- | Whether the module uses Data.Map (M.Map, M.fromList, M.empty)
    haskellModuleMetadataUsesMap :: Bool,
    -- | Whether the module uses Data.Set (S.Set, S.fromList, S.empty)
    haskellModuleMetadataUsesSet :: Bool}
  deriving (Eq, Ord, Read, Show)

_HaskellModuleMetadata = Core.Name "hydra.haskell.environment.HaskellModuleMetadata"

_HaskellModuleMetadata_usesByteString = Core.Name "usesByteString"

_HaskellModuleMetadata_usesInt = Core.Name "usesInt"

_HaskellModuleMetadata_usesMap = Core.Name "usesMap"

_HaskellModuleMetadata_usesSet = Core.Name "usesSet"
