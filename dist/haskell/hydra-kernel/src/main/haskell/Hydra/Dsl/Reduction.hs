-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.reduction

module Hydra.Dsl.Reduction where

import qualified Hydra.Arity as Arity
import qualified Hydra.Ast as Ast
import qualified Hydra.Checking as Checking
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Arity as DslArity
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Checking as DslChecking
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as DslErrorChecking
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
import qualified Hydra.Dsl.Resolution as DslResolution
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
import qualified Hydra.Dsl.Variables as DslVariables
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as ErrorChecking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Hoisting as Hoisting
import qualified Hydra.Inference as Inference
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Relational as Relational
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Rewriting as Rewriting
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
import qualified Hydra.Variables as Variables
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL reference to hydra.reduction.alphaConvert
alphaConvert :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
alphaConvert arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.alphaConvert")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.reduction.betaReduceType
betaReduceType :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Either Errors.Error Core.Type)
betaReduceType arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.betaReduceType")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.reduction.contractTerm
contractTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
contractTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.contractTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.reduction.countPrimitiveInvocations
countPrimitiveInvocations :: Typed.TypedTerm Bool
countPrimitiveInvocations = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.reduction.countPrimitiveInvocations"))

-- | DSL reference to hydra.reduction.etaExpandTerm
etaExpandTerm :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
etaExpandTerm arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.etaExpandTerm")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.reduction.etaExpandTypedTerm
etaExpandTypedTerm :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error Core.Term)
etaExpandTypedTerm arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.etaExpandTypedTerm")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.reduction.etaExpansionArity
etaExpansionArity :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Int
etaExpansionArity arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.etaExpansionArity")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.reduction.etaReduceTerm
etaReduceTerm :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Term
etaReduceTerm arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.etaReduceTerm")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.reduction.reduceTerm
reduceTerm :: Typed.TypedTerm t0 -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Bool -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Either Errors.Error Core.Term)
reduceTerm arg0 arg1 arg2 arg3 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.reduceTerm")),
            Core.applicationArgument = (Typed.unTypedTerm arg0)})),
          Core.applicationArgument = (Typed.unTypedTerm arg1)})),
        Core.applicationArgument = (Typed.unTypedTerm arg2)})),
      Core.applicationArgument = (Typed.unTypedTerm arg3)}))

-- | DSL reference to hydra.reduction.termIsClosed
termIsClosed :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
termIsClosed arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.termIsClosed")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.reduction.termIsValue
termIsValue :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Bool
termIsValue arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.reduction.termIsValue")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
