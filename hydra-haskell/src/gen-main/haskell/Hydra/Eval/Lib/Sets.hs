-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Set functions for the Hydra interpreter.

module Hydra.Eval.Lib.Sets where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Interpreter-friendly set difference.
difference :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
difference cx g set1Term set2Term =
    Eithers.bind (Core_.set cx g set1Term) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
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
intersection :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
intersection cx g set1Term set2Term =
    Eithers.bind (Core_.set cx g set1Term) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
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
map :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
map cx g fun setTerm =
    Eithers.bind (Core_.set cx g setTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList")),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = fun,
        Core.applicationArgument = el})) (Sets.toList elements)))})))

-- | Interpreter-friendly set union.
union :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
union cx g set1Term set2Term =
    Eithers.bind (Core_.set cx g set1Term) (\elements -> Right (Lists.foldl (\acc -> \el -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
        Core.applicationArgument = el})),
      Core.applicationArgument = acc})) set2Term (Sets.toList elements)))

-- | Interpreter-friendly unions for list of Set terms.
unions :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
unions cx g listTerm =
    Eithers.bind (Core_.list cx g listTerm) (\elements -> Right (Lists.foldl (\acc -> \s -> Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.union")),
        Core.applicationArgument = acc})),
      Core.applicationArgument = s})) (Core.TermSet (Sets.fromList [])) elements))
