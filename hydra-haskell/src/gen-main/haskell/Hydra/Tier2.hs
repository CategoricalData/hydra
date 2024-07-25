-- | A module for miscellaneous tier-2 functions and constants.

module Hydra.Tier2 where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
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

-- | Get the annotated type of a given term, if any
getTermType :: (Core.Term -> Maybe Core.Type)
getTermType x = case x of
  Core.TermAnnotated v275 -> (getTermType (Core.annotatedSubject v275))
  Core.TermTyped v276 -> (Just (Core.termWithTypeType v276))
  _ -> Nothing

-- | Set the state of a flow
putState :: (s -> Compute.Flow s ())
putState cx = (Compute.Flow (\s0 -> \t0 ->  
  let f1 = (Compute.unFlow (Flows.pure ()) s0 t0)
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = cx,
    Compute.flowStateTrace = (Compute.flowStateTrace f1)}))

-- | Get the annotated type of a given element, or fail if it is missing
requireElementType :: (Graph.Element -> Compute.Flow Graph.Graph Core.Type)
requireElementType el =  
  let withType = (\x -> case x of
          Nothing -> (Flows.fail (Strings.cat [
            "missing type annotation for element ",
            (Core.unName (Graph.elementName el))]))
          Just v277 -> (Flows.pure v277))
  in (withType (getTermType (Graph.elementData el)))

-- | Get the annotated type of a given term, or fail if it is missing
requireTermType :: (Core.Term -> Compute.Flow Graph.Graph Core.Type)
requireTermType x = (withType (getTermType x)) 
  where 
    withType = (\x -> case x of
      Nothing -> (Flows.fail "missing type annotation")
      Just v278 -> (Flows.pure v278))

-- | Fail if an actual value does not match an expected value
unexpected :: (String -> String -> Compute.Flow s x)
unexpected expected actual = (Flows.fail (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      expected],
    " but found: "],
  actual]))