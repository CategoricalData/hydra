-- | Utilities for working with errors and flow state

module Hydra.Errors where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Flows as Flows
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

exec :: (Compute.Flow t1 t0 -> t1 -> t1)
exec f s0 = (Compute.flowStateState (Compute.unFlow f s0 Flows.emptyTrace))

getState :: (Compute.Flow t0 t0)
getState = (Compute.Flow (\s0 -> \t0 ->  
  let fs1 = (Compute.unFlow (Flows_.pure ()) s0 t0)
  in ((\v -> \s -> \t -> Optionals.maybe (Compute.FlowState {
    Compute.flowStateValue = Nothing,
    Compute.flowStateState = s,
    Compute.flowStateTrace = t}) (\_ -> Compute.FlowState {
    Compute.flowStateValue = (Just s),
    Compute.flowStateState = s,
    Compute.flowStateTrace = t}) v) (Compute.flowStateValue fs1) (Compute.flowStateState fs1) (Compute.flowStateTrace fs1))))

modify :: ((t0 -> t0) -> Compute.Flow t0 ())
modify f = (Flows_.bind getState (\s -> putState (f s)))

putState :: (t0 -> Compute.Flow t0 ())
putState cx = (Compute.Flow (\s0 -> \t0 ->  
  let f1 = (Compute.unFlow (Flows_.pure ()) s0 t0)
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = cx,
    Compute.flowStateTrace = (Compute.flowStateTrace f1)}))

-- | Summarize a trace as a string
traceSummary :: (Compute.Trace -> String)
traceSummary t =  
  let messageLines = (Lists.nub (Compute.traceMessages t)) 
      keyvalLines = (Logic.ifElse (Maps.null (Compute.traceOther t)) [] (Lists.cons "key/value pairs: " (Lists.map toLine (Maps.toList (Compute.traceOther t)))))
      toLine = (\pair -> Strings.cat [
              Strings.cat [
                Strings.cat [
                  "\t",
                  (Core.unName (fst pair))],
                ": "],
              (Core_.showTerm (snd pair))])
  in (Strings.intercalate "\n" (Lists.concat2 messageLines keyvalLines))

unexpected :: (String -> String -> Compute.Flow t0 t1)
unexpected expected actual = (Flows_.fail (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      expected],
    " but found: "],
  actual]))
