{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Errors where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Messages     as Messages
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Accessors as Accessors
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
import qualified Hydra.Sources.Tier2.Variants as Variants


hydraErrorsModule :: Module
hydraErrorsModule = Module (Namespace "hydra.errors") elements
    [Variants.hydraVariantsModule, Monads.hydraMonadsModule, ShowCore.showCoreModule]
    [Tier1.hydraComputeModule, Tier1.hydraMantleModule] $
    Just "Utilities for working with errors and flow state"
  where
   elements = [
      el execDef,
      el getStateDef,
      el modifyDef,
      el putStateDef,
      el traceSummaryDef,
      el unexpectedDef]

errorsDefinition :: String -> TTerm a -> TElement a
errorsDefinition = definitionInModule hydraErrorsModule

execDef :: TElement (Flow s a -> s -> s)
execDef = errorsDefinition "exec" $
  lambdas ["f", "s0"] $
    Compute.flowStateState $ Compute.unFlow (var "f") (var "s0") (ref Monads.emptyTraceDef)

getStateDef :: TElement (Flow s s)
getStateDef = errorsDefinition "getState" $ -- Flow s s
  doc "Get the state of the current flow" $
  wrap _Flow $ lambda "s0" $ lambda "t0" $ lets [
    "fs1">: Compute.unFlow (Flows.pure unit) (var "s0") (var "t0")] $ -- FlowState s ()
    (lambda "v" $ lambda "s" $ lambda "t" $ (
        (primitive _optionals_maybe
          @@ (Compute.flowState nothing (var "s") (var "t"))
          @@ (constant (Compute.flowState (just $ var "s") (var "s") (var "t"))))
         @@ var "v"))
      @@ (Compute.flowStateValue $ var "fs1") @@ (Compute.flowStateState $ var "fs1") @@ (Compute.flowStateTrace $ var "fs1")

modifyDef :: TElement ((s -> s) -> Flow s ())
modifyDef = errorsDefinition "modify" $
  lambda "f" $
    Flows.bind
      (ref getStateDef)
      (lambda "s" $ ref putStateDef @@ (var "f" @@ var "s"))

putStateDef :: TElement (s -> Flow s ())
putStateDef = errorsDefinition "putState" $
  doc "Set the state of a flow" $
  lambda "cx" $ wrap _Flow $ lambda "s0" $ lambda "t0" $ lets [
    "f1">: Compute.unFlow (Flows.pure unit) (var "s0") (var "t0")] $
    Compute.flowState
      (Compute.flowStateValue $ var "f1")
      (var "cx")
      (Compute.flowStateTrace $ var "f1")

traceSummaryDef :: TElement (Trace -> String)
traceSummaryDef = errorsDefinition "traceSummary" $
  doc "Summarize a trace as a string" $
  lambda "t" $ lets [
    "messageLines">: (Lists.nub (Compute.traceMessages $ var "t")),
    "keyvalLines">: Logic.ifElse (Maps.null (Compute.traceOther $ var "t"))
      (list [])
      (Lists.cons ("key/value pairs: ")
        (Lists.map (var "toLine") (Maps.toList (Compute.traceOther $ var "t")))),
    "toLine">:
      lambda "pair" $ "\t" ++ (Core.unName $ (first $ var "pair")) ++ ": " ++ (ref ShowCore.termDef @@ (second $ var "pair"))] $
    Strings.intercalate "\n" (Lists.concat2 (var "messageLines") (var "keyvalLines"))

unexpectedDef :: TElement (String -> String -> Flow s x)
unexpectedDef = errorsDefinition "unexpected" $
  doc "Fail if an actual value does not match an expected value" $
  lambda "expected" $ lambda "actual" $ Flows.fail ("expected " ++ var "expected" ++ " but found: " ++ var "actual")
