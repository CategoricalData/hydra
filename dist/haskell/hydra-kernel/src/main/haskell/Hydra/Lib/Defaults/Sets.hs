-- Note: this is an automatically generated file. Do not edit.
-- | Default term-level implementations of Set functions for the Hydra interpreter.

module Hydra.Lib.Defaults.Sets where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Interpreter-friendly set difference.
difference :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
difference cx g set1Term set2Term =
    Eithers.bind (ExtractCore.set g set1Term) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.member")),
              Core.applicationArgument = el})),
            Core.applicationArgument = set2Term}))})),
        Core.applicationArgument = acc})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
          Core.applicationArgument = el})),
        Core.applicationArgument = acc}))})) (Core.TermSet (Sets.fromList [])) (Sets.toList elements)))
-- | Interpreter-friendly set intersection.
intersection :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
intersection cx g set1Term set2Term =
    Eithers.bind (ExtractCore.set g set1Term) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.member")),
              Core.applicationArgument = el})),
            Core.applicationArgument = set2Term}))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
            Core.applicationArgument = el})),
          Core.applicationArgument = acc}))})),
      Core.applicationArgument = acc})) (Core.TermSet (Sets.fromList [])) (Sets.toList elements)))
-- | Interpreter-friendly map for Set terms.
map :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
map cx g fun setTerm =
    Eithers.bind (ExtractCore.set g setTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList")),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = fun,
        Core.applicationArgument = el})) (Sets.toList elements)))})))
-- | Interpreter-friendly set union.
union :: t0 -> Graph.Graph -> Core.Term -> Core.Term -> Either Errors.Error Core.Term
union cx g set1Term set2Term =
    Eithers.bind (ExtractCore.set g set1Term) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
        Core.applicationArgument = el})),
      Core.applicationArgument = acc})) set2Term (Sets.toList elements)))
-- | Interpreter-friendly unions for list of Set terms.
unions :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
unions cx g listTerm =
    Eithers.bind (ExtractCore.list g listTerm) (\elements -> Right (Lists.foldl (\acc -> \s -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.union")),
        Core.applicationArgument = acc})),
      Core.applicationArgument = s})) (Core.TermSet (Sets.fromList [])) elements))
