-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Set functions for the Hydra interpreter.

module Hydra.Eval.Lib.Sets where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly map for Set terms.
map :: Context.Context -> Graph.Graph -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term
map cx g fun setTerm =
    Eithers.bind (Core_.set cx g setTerm) (\elements -> Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
      Core.applicationArgument = (Core.TermList (Lists.map (\el -> Core.TermApplication (Core.Application {
        Core.applicationFunction = fun,
        Core.applicationArgument = el})) (Sets.toList elements)))})))
