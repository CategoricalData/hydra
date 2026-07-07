-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.rewriting

module Hydra.Dsl.Rewriting where

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
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
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

-- | DSL reference to hydra.rewriting.applyInsideTypeLambdasAndAnnotations
applyInsideTypeLambdasAndAnnotations :: Typed.TypedTerm (Core.Term -> Core.Term) -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
applyInsideTypeLambdasAndAnnotations arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.applyInsideTypeLambdasAndAnnotations")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.rewriting.foldOverTerm
foldOverTerm :: Typed.TypedTerm Coders.TraversalOrder -> Typed.TypedTerm (t0 -> Core.Term -> t0) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm t0
foldOverTerm arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.foldOverTerm")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.rewriting.foldOverType
foldOverType :: Typed.TypedTerm Coders.TraversalOrder -> Typed.TypedTerm (t0 -> Core.Type -> t0) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Type -> Typed.TypedTerm t0
foldOverType arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.foldOverType")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.rewriting.foldTermWithGraphAndPath
foldTermWithGraphAndPath :: Typed.TypedTerm ((t0 -> Core.Term -> t0) -> [Paths.SubtermStep] -> Graph.Graph -> t0 -> Core.Term -> t0) -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm t0
foldTermWithGraphAndPath arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.foldTermWithGraphAndPath")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.rewriting.mapBeneathTypeAnnotations
mapBeneathTypeAnnotations :: Typed.TypedTerm (Core.Type -> Core.Type) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
mapBeneathTypeAnnotations arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.mapBeneathTypeAnnotations")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.rewriting.rewriteAndFoldTerm
rewriteAndFoldTerm :: Typed.TypedTerm ((t0 -> Core.Term -> (t0, Core.Term)) -> t0 -> Core.Term -> (t0, Core.Term)) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (t0, Core.Term)
rewriteAndFoldTerm arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteAndFoldTerm")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.rewriting.rewriteAndFoldTermWithGraph
rewriteAndFoldTermWithGraph :: Typed.TypedTerm ((t0 -> Core.Term -> (t0, Core.Term)) -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)) -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (t0, Core.Term)
rewriteAndFoldTermWithGraph arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteAndFoldTermWithGraph")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.rewriting.rewriteAndFoldTermWithGraphAndPath
rewriteAndFoldTermWithGraphAndPath :: Typed.TypedTerm ((t0 -> Core.Term -> (t0, Core.Term)) -> [Paths.SubtermStep] -> Graph.Graph -> t0 -> Core.Term -> (t0, Core.Term)) -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (t0, Core.Term)
rewriteAndFoldTermWithGraphAndPath arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteAndFoldTermWithGraphAndPath")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.rewriting.rewriteAndFoldTermWithPath
rewriteAndFoldTermWithPath :: Typed.TypedTerm (([Paths.SubtermStep] -> t0 -> Core.Term -> (t0, Core.Term)) -> [Paths.SubtermStep] -> t0 -> Core.Term -> (t0, Core.Term)) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (t0, Core.Term)
rewriteAndFoldTermWithPath arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteAndFoldTermWithPath")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.rewriting.rewriteTerm
rewriteTerm :: Typed.TypedTerm ((Core.Term -> Core.Term) -> Core.Term -> Core.Term) -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
rewriteTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.rewriting.rewriteTermM
rewriteTermM :: Typed.TypedTerm ((Core.Term -> Either t0 Core.Term) -> Core.Term -> Either t0 Core.Term) -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either t0 Core.Term)
rewriteTermM arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteTermM")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.rewriting.rewriteTermWithContext
rewriteTermWithContext :: Typed.TypedTerm ((t0 -> Core.Term -> Core.Term) -> t0 -> Core.Term -> Core.Term) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
rewriteTermWithContext arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteTermWithContext")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.rewriting.rewriteTermWithContextM
rewriteTermWithContextM :: Typed.TypedTerm ((t0 -> Core.Term -> Either t1 Core.Term) -> t0 -> Core.Term -> Either t1 Core.Term) -> Typed.TypedTerm t0 -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either t1 Core.Term)
rewriteTermWithContextM arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteTermWithContextM")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.rewriting.rewriteTermWithGraph
rewriteTermWithGraph :: Typed.TypedTerm ((Core.Term -> t0) -> Graph.Graph -> Core.Term -> t0) -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm t0
rewriteTermWithGraph arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteTermWithGraph")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.rewriting.rewriteType
rewriteType :: Typed.TypedTerm ((Core.Type -> Core.Type) -> Core.Type -> Core.Type) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type
rewriteType arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteType")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.rewriting.rewriteTypeM
rewriteTypeM :: Typed.TypedTerm ((Core.Type -> Either t0 Core.Type) -> Core.Type -> Either t0 Core.Type) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Either t0 Core.Type)
rewriteTypeM arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.rewriteTypeM")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.rewriting.subterms
subterms :: Typed.TypedTerm Core.Term -> Typed.TypedTerm [Core.Term]
subterms arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.subterms")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.rewriting.subtermsWithSteps
subtermsWithSteps :: Typed.TypedTerm Core.Term -> Typed.TypedTerm [(Paths.SubtermStep, Core.Term)]
subtermsWithSteps arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.subtermsWithSteps")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.rewriting.subtypes
subtypes :: Typed.TypedTerm Core.Type -> Typed.TypedTerm [Core.Type]
subtypes arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.rewriting.subtypes")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
