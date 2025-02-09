-- | Utilities for working with errors and flow state

module Hydra.Errors where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Io as Io
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Strings as Strings
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Get the state of the current flow
getState :: (Compute.Flow s s)
getState = (Compute.Flow (\s0 -> \t0 ->  
  let fs1 = (Compute.unFlow (Flows.pure ()) s0 t0)
  in ((\v -> \s -> \t -> (\x -> case x of
    Nothing -> Compute.FlowState {
      Compute.flowStateValue = Nothing,
      Compute.flowStateState = s,
      Compute.flowStateTrace = t}
    Just _ -> Compute.FlowState {
      Compute.flowStateValue = (Just s),
      Compute.flowStateState = s,
      Compute.flowStateTrace = t}) v) (Compute.flowStateValue fs1) (Compute.flowStateState fs1) (Compute.flowStateTrace fs1))))

-- | Set the state of a flow
putState :: (s -> Compute.Flow s ())
putState cx = (Compute.Flow (\s0 -> \t0 ->  
  let f1 = (Compute.unFlow (Flows.pure ()) s0 t0)
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = cx,
    Compute.flowStateTrace = (Compute.flowStateTrace f1)}))

-- | Summarize a trace as a string
traceSummary :: (Compute.Trace -> String)
traceSummary t =  
  let messageLines = (Lists.nub (Compute.traceMessages t)) 
      keyvalLines = (Logic.ifElse [] (Lists.cons "key/value pairs: " (Lists.map toLine (Maps.toList (Compute.traceOther t)))) (Maps.isEmpty (Compute.traceOther t)))
      toLine = (\pair -> Strings.cat [
              Strings.cat [
                Strings.cat [
                  "\t",
                  (Core.unName (fst pair))],
                ": "],
              (Io.showTerm (snd pair))])
  in (Strings.intercalate "\n" (Lists.concat2 messageLines keyvalLines))

-- | Fail if an actual value does not match an expected value
unexpected :: (String -> String -> Compute.Flow s x)
unexpected expected actual = (Flows.fail (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      expected],
    " but found: "],
  actual]))
