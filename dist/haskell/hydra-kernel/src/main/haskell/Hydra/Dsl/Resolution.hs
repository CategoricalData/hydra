-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.resolution

module Hydra.Dsl.Resolution where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.File as DslErrorFile
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Error.System as DslErrorSystem
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Lexical as DslLexical
import qualified Hydra.Dsl.Names as DslNames
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Scoping as DslScoping
import qualified Hydra.Dsl.Strip as DslStrip
import qualified Hydra.Dsl.System as DslSystem
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
import qualified Hydra.Dsl.Variables as DslVariables
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import qualified Hydra.Substitution as Substitution
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variables as Variables
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | DSL reference to hydra.resolution.dereferenceType
dereferenceType :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error (Maybe Core.Type))
dereferenceType arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.dereferenceType")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.resolution.fTypeIsPolymorphic
fTypeIsPolymorphic :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
fTypeIsPolymorphic arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.fTypeIsPolymorphic")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.resolution.fieldMap
fieldMap :: Typed.TypedTerm [Core.Field] -> Typed.TypedTerm (M.Map Core.Name Core.Term)
fieldMap arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.fieldMap")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.resolution.fieldTypeMap
fieldTypeMap :: Typed.TypedTerm [Core.FieldType] -> Typed.TypedTerm (M.Map Core.Name Core.Type)
fieldTypeMap arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.fieldTypeMap")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.resolution.fieldTypes
fieldTypes :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Either Errors.Error (M.Map Core.Name Core.Type))
fieldTypes arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.fieldTypes")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.resolution.findFieldType
findFieldType :: Typed.TypedTerm t0 -> Typed.TypedTerm Core.Name -> Typed.TypedTerm [Core.FieldType] -> Typed.TypedTerm (Either Errors.Error Core.Type)
findFieldType arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.findFieldType")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.resolution.fullyStripAndNormalizeType
fullyStripAndNormalizeType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
fullyStripAndNormalizeType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.fullyStripAndNormalizeType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.resolution.fullyStripType
fullyStripType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
fullyStripType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.fullyStripType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.resolution.instantiateType
instantiateType :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Core.Type, Typing.InferenceContext)
instantiateType arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.instantiateType")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.resolution.instantiateTypeScheme
instantiateTypeScheme :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm (Core.TypeScheme, Typing.InferenceContext)
instantiateTypeScheme arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.instantiateTypeScheme")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.resolution.nominalApplication
nominalApplication :: Typed.TypedTerm Core.Name -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm Core.Type
nominalApplication arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.nominalApplication")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.resolution.requireRecordType
requireRecordType :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error [Core.FieldType])
requireRecordType arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.requireRecordType")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.resolution.requireRowType
requireRowType :: Typed.TypedTerm t0 -> Typed.TypedTerm String -> Typed.TypedTerm (Core.Type -> Maybe t1) -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error t1)
requireRowType arg0 arg1 arg2 arg3 arg4 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.requireRowType")),
              Core.applicationArgument = (Typed.unTypedTerm arg0)})),
            Core.applicationArgument = (Typed.unTypedTerm arg1)})),
          Core.applicationArgument = (Typed.unTypedTerm arg2)})),
        Core.applicationArgument = (Typed.unTypedTerm arg3)})),
      Core.applicationArgument = (Typed.unTypedTerm arg4)}))

-- | DSL reference to hydra.resolution.requireSchemaType
requireSchemaType :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error (Core.TypeScheme, Typing.InferenceContext))
requireSchemaType arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.requireSchemaType")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.resolution.requireType
requireType :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Core.Type)
requireType arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.requireType")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.resolution.requireUnionField
requireUnionField :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Core.Type)
requireUnionField arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.requireUnionField")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.resolution.requireUnionType
requireUnionType :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error [Core.FieldType])
requireUnionType arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.requireUnionType")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.resolution.resolveType
resolveType :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Maybe Core.Type)
resolveType arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.resolveType")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.resolution.typeToTypeScheme
typeToTypeScheme :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.TypeScheme
typeToTypeScheme arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.resolution.typeToTypeScheme")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
