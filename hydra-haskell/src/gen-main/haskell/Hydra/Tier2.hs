-- | A module for miscellaneous tier-2 functions and constants.

module Hydra.Tier2 where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import Data.Int
import Data.List
import Data.Map
import Data.Set

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

requireTypeAnnotation :: (Core.Term a -> Compute.Flow (Graph.Graph a) (Core.Type a))
requireTypeAnnotation term =  
  let annsToType = (\anns -> Flows.bind (Graph.annotationClassTermType anns term) checkType) 
      checkType = (\x -> case x of
              Nothing -> (Flows.fail "missing type annotation")
              Just v -> (Flows.pure v))
  in (Flows.bind (Flows.map Graph.graphAnnotations getState) annsToType)