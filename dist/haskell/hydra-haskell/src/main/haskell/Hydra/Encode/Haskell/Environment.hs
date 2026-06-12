-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.haskell.environment

module Hydra.Encode.Haskell.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Environment as Environment
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadata :: Environment.HaskellModuleMetadata -> Core.Term
haskellModuleMetadata x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Environment.haskellModuleMetadataUsesByteString x))},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Environment.haskellModuleMetadataUsesInt x))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Environment.haskellModuleMetadataUsesMap x))},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Environment.haskellModuleMetadataUsesSet x))}]})
