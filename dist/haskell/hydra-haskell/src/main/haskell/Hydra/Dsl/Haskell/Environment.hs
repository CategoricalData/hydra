-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.haskell.environment

module Hydra.Dsl.Haskell.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Environment as Environment
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
haskellModuleMetadata :: Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.HaskellModuleMetadata
haskellModuleMetadata usesByteString usesInt usesMap usesSet =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Phantoms.unTTerm usesByteString)},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Phantoms.unTTerm usesInt)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Phantoms.unTTerm usesMap)},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Phantoms.unTTerm usesSet)}]}))
haskellModuleMetadataUsesByteString :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool
haskellModuleMetadataUsesByteString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionField = (Core.Name "usesByteString")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
haskellModuleMetadataUsesInt :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool
haskellModuleMetadataUsesInt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionField = (Core.Name "usesInt")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
haskellModuleMetadataUsesMap :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool
haskellModuleMetadataUsesMap x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionField = (Core.Name "usesMap")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
haskellModuleMetadataUsesSet :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool
haskellModuleMetadataUsesSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
        Core.projectionField = (Core.Name "usesSet")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
haskellModuleMetadataWithUsesByteString :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesByteString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesInt")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesMap")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
haskellModuleMetadataWithUsesInt :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesInt original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesByteString")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesMap")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
haskellModuleMetadataWithUsesMap :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesMap original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesByteString")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesInt")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesSet")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
haskellModuleMetadataWithUsesSet :: Phantoms.TTerm Environment.HaskellModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.HaskellModuleMetadata
haskellModuleMetadataWithUsesSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "usesByteString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesByteString")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesInt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesInt")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMap"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.environment.HaskellModuleMetadata"),
              Core.projectionField = (Core.Name "usesMap")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesSet"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
