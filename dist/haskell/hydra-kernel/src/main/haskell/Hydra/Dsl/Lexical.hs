-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.lexical

module Hydra.Dsl.Lexical where

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
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Errors as ShowErrors
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
import qualified Data.Set as S

-- | DSL reference to hydra.lexical.buildGraph
buildGraph :: Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm (M.Map Core.Name (Maybe Core.Term)) -> Typed.TypedTerm (M.Map Core.Name Graph.Primitive) -> Typed.TypedTerm Graph.Graph
buildGraph arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.buildGraph")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.lexical.chooseUniqueName
chooseUniqueName :: Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name
chooseUniqueName arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.chooseUniqueName")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.dereferenceSchemaType
dereferenceSchemaType :: Typed.TypedTerm Core.Name -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm (Maybe Core.TypeScheme)
dereferenceSchemaType arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.dereferenceSchemaType")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.dereferenceVariable
dereferenceVariable :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Core.Binding)
dereferenceVariable arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.dereferenceVariable")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.elementsToGraph
elementsToGraph :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm Graph.Graph
elementsToGraph arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.elementsToGraph")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.lexical.emptyGraph
emptyGraph :: Typed.TypedTerm Graph.Graph
emptyGraph = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.lexical.emptyGraph"))

-- | DSL reference to hydra.lexical.emptyInferenceContext
emptyInferenceContext :: Typed.TypedTerm Typing.InferenceContext
emptyInferenceContext = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.lexical.emptyInferenceContext"))

-- | DSL reference to hydra.lexical.fieldsOf
fieldsOf :: Typed.TypedTerm Core.Type -> Typed.TypedTerm [Core.FieldType]
fieldsOf arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.fieldsOf")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lexical.getField
getField :: Typed.TypedTerm (M.Map Core.Name t0) -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (t0 -> Either Errors.Error t1) -> Typed.TypedTerm (Either Errors.Error t1)
getField arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.getField")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.lexical.graphToBindings
graphToBindings :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Binding]
graphToBindings arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.graphToBindings")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lexical.graphWithPrimitives
graphWithPrimitives :: Typed.TypedTerm [Graph.Primitive] -> Typed.TypedTerm [Graph.Primitive] -> Typed.TypedTerm Graph.Graph
graphWithPrimitives arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.graphWithPrimitives")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.lookupBinding
lookupBinding :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Core.Binding)
lookupBinding arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.lookupBinding")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.lookupPrimitive
lookupPrimitive :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Graph.Primitive)
lookupPrimitive arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.lookupPrimitive")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.lookupTerm
lookupTerm :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Core.Term)
lookupTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.lookupTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.matchEnum
matchEnum :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm [(Core.Name, t0)] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error t0)
matchEnum arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.matchEnum")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.lexical.matchRecord
matchRecord :: Typed.TypedTerm t0 -> Typed.TypedTerm (M.Map Core.Name Core.Term -> Either Errors.Error t1) -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error t1)
matchRecord arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.matchRecord")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.lexical.matchUnion
matchUnion :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm [(Core.Name, (Core.Term -> Either Errors.Error t0))] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error t0)
matchUnion arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.matchUnion")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.lexical.matchUnitField
matchUnitField :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm (t0, (t2 -> Either t3 t1))
matchUnitField arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.matchUnitField")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.requireBinding
requireBinding :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Core.Binding)
requireBinding arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.requireBinding")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.requirePrimitive
requirePrimitive :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Graph.Primitive)
requirePrimitive arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.requirePrimitive")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.requirePrimitiveType
requirePrimitiveType :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Core.TypeScheme)
requirePrimitiveType arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.requirePrimitiveType")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.requireTerm
requireTerm :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Either Errors.Error Core.Term)
requireTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.requireTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.resolveTerm
resolveTerm :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Core.Term)
resolveTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.resolveTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.stripAndDereferenceTerm
stripAndDereferenceTerm :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error Core.Term)
stripAndDereferenceTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.lexical.stripAndDereferenceTermEither
stripAndDereferenceTermEither :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error Core.Term)
stripAndDereferenceTermEither arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
