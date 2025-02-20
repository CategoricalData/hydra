-- | Functions for working with flows (the Hydra state monad).

module Hydra.Flows where

import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

bind :: (Compute.Flow s a -> (a -> Compute.Flow s b) -> Compute.Flow s b)
bind l r =  
  let q = (\s0 -> \t0 ->  
          let fs1 = (Compute.unFlow l s0 t0)
          in ((\x -> case x of
            Nothing -> Compute.FlowState {
              Compute.flowStateValue = Nothing,
              Compute.flowStateState = (Compute.flowStateState fs1),
              Compute.flowStateTrace = (Compute.flowStateTrace fs1)}
            Just v1 -> (Compute.unFlow (r v1) (Compute.flowStateState fs1) (Compute.flowStateTrace fs1))) (Compute.flowStateValue fs1)))
  in (Compute.Flow q)

-- | Bind the results of two flows into another flow
bind2 :: (Compute.Flow s a -> Compute.Flow s b -> (a -> b -> Compute.Flow s c) -> Compute.Flow s c)
bind2 f1 f2 f = (bind f1 (\r1 -> bind f2 (\r2 -> f r1 r2)))

-- | Bind the results of three flows into another flow
bind3 :: (Compute.Flow s a -> Compute.Flow s b -> Compute.Flow s c -> (a -> b -> c -> Compute.Flow s d) -> Compute.Flow s d)
bind3 f1 f2 f3 f = (bind f1 (\r1 -> bind f2 (\r2 -> bind f3 (\r3 -> f r1 r2 r3))))

-- | Bind the results of four flows into another flow
bind4 :: (Compute.Flow s a -> Compute.Flow s b -> Compute.Flow s c -> Compute.Flow s d -> (a -> b -> c -> d -> Compute.Flow s e) -> Compute.Flow s e)
bind4 f1 f2 f3 f4 f = (bind f1 (\r1 -> bind f2 (\r2 -> bind f3 (\r3 -> bind f4 (\r4 -> f r1 r2 r3 r4)))))

emptyTrace :: Compute.Trace
emptyTrace = Compute.Trace {
  Compute.traceStack = [],
  Compute.traceMessages = [],
  Compute.traceOther = Maps.empty}

failInternal :: (String -> Compute.Flow s a)
failInternal msg = (Compute.Flow (\s -> \t -> Compute.FlowState {
  Compute.flowStateValue = Nothing,
  Compute.flowStateState = s,
  Compute.flowStateTrace = (pushError msg t)}))

-- | Check whether a flow succeeds
flowSucceeds :: (s -> Compute.Flow s a -> Bool)
flowSucceeds cx f = (Optionals.isJust (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

-- | Get the value of a flow, or a default value if the flow fails
fromFlow :: (a -> s -> Compute.Flow s a -> a)
fromFlow def cx f = ((\x -> case x of
  Nothing -> def
  Just v1 -> v1) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

mutateTrace :: ((Compute.Trace -> Mantle.Either_ String Compute.Trace) -> (Compute.Trace -> Compute.Trace -> Compute.Trace) -> Compute.Flow s a -> Compute.Flow s a)
mutateTrace mutate restore f = (Compute.Flow (\s0 -> \t0 ->  
  let forLeft = (\msg -> Compute.FlowState {
          Compute.flowStateValue = Nothing,
          Compute.flowStateState = s0,
          Compute.flowStateTrace = (pushError msg t0)}) 
      forRight = (\t1 ->  
              let f2 = (Compute.unFlow f s0 t1)
              in Compute.FlowState {
                Compute.flowStateValue = (Compute.flowStateValue f2),
                Compute.flowStateState = (Compute.flowStateState f2),
                Compute.flowStateTrace = (restore t0 (Compute.flowStateTrace f2))})
  in ((\x -> case x of
    Mantle.EitherLeft v1 -> (forLeft v1)
    Mantle.EitherRight v1 -> (forRight v1)) (mutate t0))))

pureInternal :: (a -> Compute.Flow s a)
pureInternal x = (Compute.Flow (\s -> \t -> Compute.FlowState {
  Compute.flowStateValue = (Just x),
  Compute.flowStateState = s,
  Compute.flowStateTrace = t}))

-- | Push an error message
pushError :: (String -> Compute.Trace -> Compute.Trace)
pushError msg t =  
  let errorMsg = (Strings.cat [
          "Error: ",
          msg,
          " (",
          Strings.intercalate " > " (Lists.reverse (Compute.traceStack t)),
          ")"])
  in Compute.Trace {
    Compute.traceStack = (Compute.traceStack t),
    Compute.traceMessages = (Lists.cons errorMsg (Compute.traceMessages t)),
    Compute.traceOther = (Compute.traceOther t)}

-- | Continue the current flow after adding a warning message
warn :: (String -> Compute.Flow s a -> Compute.Flow s a)
warn msg b = (Compute.Flow (\s0 -> \t0 ->  
  let f1 = (Compute.unFlow b s0 t0) 
      addMessage = (\t -> Compute.Trace {
              Compute.traceStack = (Compute.traceStack t),
              Compute.traceMessages = (Lists.cons (Strings.cat [
                "Warning: ",
                msg]) (Compute.traceMessages t)),
              Compute.traceOther = (Compute.traceOther t)})
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = (Compute.flowStateState f1),
    Compute.flowStateTrace = (addMessage (Compute.flowStateTrace f1))}))

-- | Continue the current flow after setting a flag
withFlag :: (Core.Name -> Compute.Flow s a -> Compute.Flow s a)
withFlag flag =  
  let mutate = (\t -> Mantle.EitherRight (Compute.Trace {
          Compute.traceStack = (Compute.traceStack t),
          Compute.traceMessages = (Compute.traceMessages t),
          Compute.traceOther = (Maps.insert flag (Core.TermLiteral (Core.LiteralBoolean True)) (Compute.traceOther t))})) 
      restore = (\ignored -> \t1 -> Compute.Trace {
              Compute.traceStack = (Compute.traceStack t1),
              Compute.traceMessages = (Compute.traceMessages t1),
              Compute.traceOther = (Maps.remove flag (Compute.traceOther t1))})
  in (mutateTrace mutate restore)

-- | Continue a flow using a given state
withState :: (s1 -> Compute.Flow s1 a -> Compute.Flow s2 a)
withState cx0 f = (Compute.Flow (\cx1 -> \t1 ->  
  let f1 = (Compute.unFlow f cx0 t1)
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = cx1,
    Compute.flowStateTrace = (Compute.flowStateTrace f1)}))

-- | Continue the current flow after augmenting the trace
withTrace :: (String -> Compute.Flow s a -> Compute.Flow s a)
withTrace msg =  
  let mutate = (\t -> Logic.ifElse (Equality.gteInt32 (Lists.length (Compute.traceStack t)) Constants.maxTraceDepth) (Mantle.EitherLeft "maximum trace depth exceeded. This may indicate an infinite loop") (Mantle.EitherRight (Compute.Trace {
          Compute.traceStack = (Lists.cons msg (Compute.traceStack t)),
          Compute.traceMessages = (Compute.traceMessages t),
          Compute.traceOther = (Compute.traceOther t)}))) 
      restore = (\t0 -> \t1 -> Compute.Trace {
              Compute.traceStack = (Compute.traceStack t0),
              Compute.traceMessages = (Compute.traceMessages t1),
              Compute.traceOther = (Compute.traceOther t1)})
  in (mutateTrace mutate restore)