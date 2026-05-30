-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.haskell.environment

module Hydra.Dsl.Haskell.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Environment as Environment
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadata :: Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.HaskellModuleMetadata
haskellModuleMetadata usesByteString usesInt usesMap usesSet =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Typed.unTypedTerm usesByteString)},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Typed.unTypedTerm usesInt)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Typed.unTypedTerm usesMap)},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Typed.unTypedTerm usesSet)}]}))
-- | DSL accessor for the usesByteString field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataUsesByteString :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool
haskellModuleMetadataUsesByteString x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesByteString")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesInt field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataUsesInt :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool
haskellModuleMetadataUsesInt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesInt")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesMap field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataUsesMap :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool
haskellModuleMetadataUsesMap x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesMap")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesSet field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataUsesSet :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool
haskellModuleMetadataUsesSet x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesSet")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the usesByteString field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesByteString :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesByteString original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesInt")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMap")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesInt field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesInt :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesInt original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesByteString")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMap")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesMap field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesMap :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesMap original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesByteString")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesInt")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesSet field of hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesSet :: Typed.TypedTerm Environment.HaskellModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesSet original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesByteString")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesInt")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMap")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
