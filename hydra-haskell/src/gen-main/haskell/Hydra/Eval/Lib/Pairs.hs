-- Note: this is an automatically generated file. Do not edit.

-- | Evaluation-level implementations of Pair functions for the Hydra interpreter.

module Hydra.Eval.Lib.Pairs where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Interpreter-friendly bimap for Pair terms.
bimap :: (Context.Context -> t0 -> Core.Term -> Core.Term -> Core.Term -> Either (Context.InContext Error.Error) Core.Term)
bimap cx g firstFun secondFun pairTerm = ((\x -> case x of
  Core.TermPair v0 ->  
    let fst = (Pairs.first v0)
    in  
      let snd = (Pairs.second v0)
      in (Right (Core.TermPair (Core.TermApplication (Core.Application {
        Core.applicationFunction = firstFun,
        Core.applicationArgument = fst}), (Core.TermApplication (Core.Application {
        Core.applicationFunction = secondFun,
        Core.applicationArgument = snd})))))
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "pair value") " but found ") (Core_.term pairTerm)))),
    Context.inContextContext = cx}))) pairTerm)
