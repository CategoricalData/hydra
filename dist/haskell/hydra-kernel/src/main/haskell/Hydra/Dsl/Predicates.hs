-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.predicates

module Hydra.Dsl.Predicates where

import qualified Hydra.Arity as Arity
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Arity as DslArity
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Dependencies as DslDependencies
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
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Rewriting as DslRewriting
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
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Query as Query
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Strip as Strip
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | DSL reference to hydra.predicates.isComplexBinding
isComplexBinding :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Binding -> Typed.TypedTerm Bool
isComplexBinding arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isComplexBinding")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.predicates.isComplexTerm
isComplexTerm :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isComplexTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isComplexTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.predicates.isComplexVariable
isComplexVariable :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Bool
isComplexVariable arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isComplexVariable")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.predicates.isEncodedTerm
isEncodedTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isEncodedTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isEncodedTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isEncodedType
isEncodedType :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isEncodedType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isEncodedType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isEnumRowType
isEnumRowType :: Typed.TypedTerm [Core.FieldType] -> Typed.TypedTerm Bool
isEnumRowType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isEnumRowType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isEnumType
isEnumType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
isEnumType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isEnumType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isNominalType
isNominalType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
isNominalType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isNominalType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isSerializable
isSerializable :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Binding -> Typed.TypedTerm (Either Errors.Error Bool)
isSerializable arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isSerializable")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.predicates.isSerializableByName
isSerializableByName :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Bool)
isSerializableByName arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isSerializableByName")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.predicates.isSerializableType
isSerializableType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
isSerializableType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isSerializableType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isTrivialTerm
isTrivialTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isTrivialTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isTrivialTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isType
isType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
isType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isUnitTerm
isUnitTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
isUnitTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isUnitTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.isUnitType
isUnitType :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool
isUnitType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.isUnitType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.predicates.typeDependencies
typeDependencies :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Bool -> Typed.TypedTerm (Core.Type -> Core.Type) -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error (M.Map Core.Name Core.Type))
typeDependencies arg0 arg1 arg2 arg3 arg4 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.predicates.typeDependencies")),
              Core.applicationArgument = (Typed.unTypedTerm arg0)})),
            Core.applicationArgument = (Typed.unTypedTerm arg1)})),
          Core.applicationArgument = (Typed.unTypedTerm arg2)})),
        Core.applicationArgument = (Typed.unTypedTerm arg3)})),
      Core.applicationArgument = (Typed.unTypedTerm arg4)}))
