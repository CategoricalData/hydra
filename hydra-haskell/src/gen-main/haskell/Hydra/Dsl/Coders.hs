-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.coders

module Hydra.Dsl.Coders where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adapterContext :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Coders.Language -> Phantoms.TTerm (M.Map Core.Name (Util.Adapter Core.Type Core.Type Core.Term Core.Term)) -> Phantoms.TTerm Coders.AdapterContext
adapterContext graph language adapters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Phantoms.unTTerm language)},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Phantoms.unTTerm adapters)}]}))

adapterContextAdapters :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm (M.Map Core.Name (Util.Adapter Core.Type Core.Type Core.Term Core.Term))
adapterContextAdapters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionField = (Core.Name "adapters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adapterContextGraph :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Graph.Graph
adapterContextGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionField = (Core.Name "graph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adapterContextLanguage :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Coders.Language
adapterContextLanguage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionField = (Core.Name "language")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adapterContextWithAdapters :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm (M.Map Core.Name (Util.Adapter Core.Type Core.Type Core.Term Core.Term)) -> Phantoms.TTerm Coders.AdapterContext
adapterContextWithAdapters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

adapterContextWithGraph :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Coders.AdapterContext
adapterContextWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "adapters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

adapterContextWithLanguage :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.AdapterContext
adapterContextWithLanguage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "adapters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

coderDirectionDecode :: Phantoms.TTerm Coders.CoderDirection
coderDirectionDecode =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decode"),
        Core.fieldTerm = Core.TermUnit}}))

coderDirectionEncode :: Phantoms.TTerm Coders.CoderDirection
coderDirectionEncode =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "encode"),
        Core.fieldTerm = Core.TermUnit}}))

language :: Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm Coders.Language
language name constraints =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)}]}))

languageConstraints :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionField = (Core.Name "constraints")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsEliminationVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.EliminationVariant)
languageConstraintsEliminationVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "eliminationVariants")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsFloatTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.FloatType)
languageConstraintsFloatTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "floatTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsFunctionVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.FunctionVariant)
languageConstraintsFunctionVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "functionVariants")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsIntegerTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.IntegerType)
languageConstraintsIntegerTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "integerTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsLiteralVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.LiteralVariant)
languageConstraintsLiteralVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "literalVariants")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsTermVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TermVariant)
languageConstraintsTermVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "termVariants")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsTypeVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TypeVariant)
languageConstraintsTypeVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "typeVariants")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (Core.Type -> Bool)
languageConstraintsTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageConstraintsWithEliminationVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.EliminationVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithEliminationVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

languageConstraintsWithFloatTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.FloatType) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithFloatTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

languageConstraintsWithFunctionVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.FunctionVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithFunctionVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

languageConstraintsWithIntegerTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.IntegerType) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithIntegerTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

languageConstraintsWithLiteralVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.LiteralVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithLiteralVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

languageConstraintsWithTermVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TermVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithTermVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

languageConstraintsWithTypeVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TypeVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithTypeVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

languageConstraintsWithTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (Core.Type -> Bool) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

languageConstraints_ :: Phantoms.TTerm (S.Set Variants.EliminationVariant) -> Phantoms.TTerm (S.Set Variants.LiteralVariant) -> Phantoms.TTerm (S.Set Core.FloatType) -> Phantoms.TTerm (S.Set Variants.FunctionVariant) -> Phantoms.TTerm (S.Set Core.IntegerType) -> Phantoms.TTerm (S.Set Variants.TermVariant) -> Phantoms.TTerm (S.Set Variants.TypeVariant) -> Phantoms.TTerm (Core.Type -> Bool) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraints_ eliminationVariants literalVariants floatTypes functionVariants integerTypes termVariants typeVariants types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Phantoms.unTTerm eliminationVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Phantoms.unTTerm literalVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Phantoms.unTTerm floatTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Phantoms.unTTerm functionVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Phantoms.unTTerm integerTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Phantoms.unTTerm termVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Phantoms.unTTerm typeVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))

languageName :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageName
languageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

languageName_ :: Phantoms.TTerm String -> Phantoms.TTerm Coders.LanguageName
languageName_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coders.LanguageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

languageWithConstraints :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm Coders.Language
languageWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

languageWithName :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm Coders.Language
languageWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionField = (Core.Name "constraints")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

traversalOrderPost :: Phantoms.TTerm Coders.TraversalOrder
traversalOrderPost =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "post"),
        Core.fieldTerm = Core.TermUnit}}))

traversalOrderPre :: Phantoms.TTerm Coders.TraversalOrder
traversalOrderPre =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pre"),
        Core.fieldTerm = Core.TermUnit}}))

unLanguageName :: Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm String
unLanguageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.coders.LanguageName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
