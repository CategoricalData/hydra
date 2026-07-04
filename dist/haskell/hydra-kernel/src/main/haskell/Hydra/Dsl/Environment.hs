-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.environment

module Hydra.Dsl.Environment where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
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
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Scoping as DslScoping
import qualified Hydra.Dsl.Sorting as DslSorting
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
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Environment as Environment
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
import qualified Hydra.Sorting as Sorting
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
import qualified Hydra.Variables as Variables
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | DSL reference to hydra.environment.definitionAsTypeApplicationTerm
definitionAsTypeApplicationTerm :: Typed.TypedTerm Core.Binding -> Typed.TypedTerm (Either Errors.Error Core.TypeApplicationTerm)
definitionAsTypeApplicationTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.definitionAsTypeApplicationTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.environment.graphAsLet
graphAsLet :: Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Let
graphAsLet arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.graphAsLet")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.environment.graphAsTerm
graphAsTerm :: Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
graphAsTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.graphAsTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.environment.graphAsTypes
graphAsTypes :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm (Either Errors.DecodingError (M.Map Core.Name Core.Type))
graphAsTypes arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.graphAsTypes")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.environment.partitionDefinitions
partitionDefinitions :: Typed.TypedTerm [Packaging.Definition] -> Typed.TypedTerm ([Packaging.TypeDefinition], [Packaging.TermDefinition])
partitionDefinitions arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.partitionDefinitions")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.environment.reorderDefs
reorderDefs :: Typed.TypedTerm [Packaging.Definition] -> Typed.TypedTerm [Packaging.Definition]
reorderDefs arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.reorderDefs")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.environment.schemaGraphToTypingEnvironment
schemaGraphToTypingEnvironment :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (Either Errors.Error (M.Map Core.Name Core.TypeScheme))
schemaGraphToTypingEnvironment arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.schemaGraphToTypingEnvironment")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.environment.termAsBindings
termAsBindings :: Typed.TypedTerm Core.Term -> Typed.TypedTerm [Core.Binding]
termAsBindings arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.termAsBindings")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.environment.typesToDefinitions
typesToDefinitions :: Typed.TypedTerm (M.Map Core.Name Core.Type) -> Typed.TypedTerm [Core.Binding]
typesToDefinitions arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.typesToDefinitions")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.environment.withLambdaContext
withLambdaContext :: Typed.TypedTerm (t0 -> Graph.Graph) -> Typed.TypedTerm (Graph.Graph -> t0 -> t1) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Lambda -> Typed.TypedTerm (t1 -> t2) -> Typed.TypedTerm t2
withLambdaContext arg0 arg1 arg2 arg3 arg4 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.withLambdaContext")),
              Core.applicationArgument = (Typed.unTypedTerm arg0)})),
            Core.applicationArgument = (Typed.unTypedTerm arg1)})),
          Core.applicationArgument = (Typed.unTypedTerm arg2)})),
        Core.applicationArgument = (Typed.unTypedTerm arg3)})),
      Core.applicationArgument = (Typed.unTypedTerm arg4)}))

-- | DSL reference to hydra.environment.withLetContext
withLetContext :: Typed.TypedTerm (t0 -> Graph.Graph) -> Typed.TypedTerm (Graph.Graph -> t0 -> t1) -> Typed.TypedTerm (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Let -> Typed.TypedTerm (t1 -> t2) -> Typed.TypedTerm t2
withLetContext arg0 arg1 arg2 arg3 arg4 arg5 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.withLetContext")),
                Core.applicationArgument = (Typed.unTypedTerm arg0)})),
              Core.applicationArgument = (Typed.unTypedTerm arg1)})),
            Core.applicationArgument = (Typed.unTypedTerm arg2)})),
          Core.applicationArgument = (Typed.unTypedTerm arg3)})),
        Core.applicationArgument = (Typed.unTypedTerm arg4)})),
      Core.applicationArgument = (Typed.unTypedTerm arg5)}))

-- | DSL reference to hydra.environment.withTypeLambdaContext
withTypeLambdaContext :: Typed.TypedTerm (t0 -> Graph.Graph) -> Typed.TypedTerm (Graph.Graph -> t0 -> t1) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.TypeLambda -> Typed.TypedTerm (t1 -> t2) -> Typed.TypedTerm t2
withTypeLambdaContext arg0 arg1 arg2 arg3 arg4 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.environment.withTypeLambdaContext")),
              Core.applicationArgument = (Typed.unTypedTerm arg0)})),
            Core.applicationArgument = (Typed.unTypedTerm arg1)})),
          Core.applicationArgument = (Typed.unTypedTerm arg2)})),
        Core.applicationArgument = (Typed.unTypedTerm arg3)})),
      Core.applicationArgument = (Typed.unTypedTerm arg4)}))
