-- | Functions and type class implementations for working with Hydra's built-in Flow monad

module Hydra.Flows where

import Hydra.Core
import Hydra.Compute
import Hydra.Graph
import Hydra.Tier1

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Control.Monad
import qualified System.IO as IO


type GraphFlow a = Flow (Graph a)

instance Functor (Flow s) where
  fmap = liftM
instance Applicative (Flow s) where
  pure = return
  (<*>) = ap
instance Monad (Flow s) where
  return x = Flow $ \s t -> FlowState (Just x) s t
  p >>= k = Flow q'
    where
      q' s0 t0 = FlowState y s2 t2
        where
          FlowState x s1 t1 = unFlow p s0 t0
          FlowState y s2 t2 = case x of
            Just x' -> unFlow (k x') s1 t1
            Nothing -> FlowState Nothing s1 t1
instance MonadFail (Flow s) where
  fail msg = Flow $ \s t -> FlowState Nothing s (pushError msg t)

fromFlowIo :: s -> Flow s a -> IO.IO a
fromFlowIo cx f = case mv of
    Just v -> pure v
    Nothing -> fail $ traceSummary trace
  where
    FlowState mv _ trace = unFlow f cx emptyTrace

getState :: Flow s s
getState = Flow q
  where
    q s0 t0 = case flowStateValue fs1 of
        Nothing -> FlowState Nothing s1 t1
        Just _ -> FlowState (Just s1) s1 t1
      where
        fs1 = unFlow (pure ()) s0 t0
        s1 = flowStateState fs1
        t1 = flowStateTrace fs1

mutateTrace :: (Trace -> Either String Trace) -> (Trace -> Trace -> Trace) -> Flow s a -> Flow s a
mutateTrace mutate restore f = Flow q
  where
    q s0 t0 = either forLeft forRight $ mutate t0
      where
        forLeft msg = FlowState Nothing s0 $ pushError msg t0
        forRight t1 = FlowState v s1 $ restore t0 t2 -- retain the updated state, but reset the trace after execution
          where
            FlowState v s1 t2 = unFlow f s0 t1 -- execute the internal flow after augmenting the trace

pushError :: String -> Trace -> Trace
pushError msg t = t {traceMessages = errorMsg:(traceMessages t)}
  where
    errorMsg = "Error: " ++ msg ++ " (" ++ L.intercalate " > " (L.reverse $ traceStack t) ++ ")"

putState :: s -> Flow s ()
putState cx = Flow q
  where
    q s0 t0 = FlowState v cx t1
      where
        FlowState v _ t1 = unFlow f s0 t0
        f = pure ()

traceSummary :: Trace -> String
traceSummary t = L.intercalate "\n" (messageLines ++ keyvalLines)
  where
    messageLines = L.nub $ traceMessages t
    keyvalLines = if M.null (traceOther t)
        then []
        else "key/value pairs:":(toLine <$> M.toList (traceOther t))
      where
        toLine (k, v) = "\t" ++ k ++ ": " ++ show v

unexpected :: Show x => String -> x -> Flow s y
unexpected cat obj = fail $ "expected " ++ cat ++ " but found: " ++ show obj

warn :: String -> Flow s a -> Flow s a
warn msg b = Flow u'
  where
    u' s0 t0 = FlowState v s1 t2
      where
        FlowState v s1 t1 = unFlow b s0 t0
        t2 = t1 {traceMessages = ("Warning: " ++ msg):(traceMessages t1)}

withFlag :: String -> Flow s a -> Flow s a
withFlag flag = mutateTrace mutate restore
  where
    mutate t = Right $ t {traceOther = M.insert flag (TermLiteral $ LiteralBoolean True) (traceOther t)}
    restore _ t1 = t1 {traceOther = M.delete flag (traceOther t1)}

withState :: s1 -> Flow s1 a -> Flow s2 a
withState cx0 f = Flow q
  where
    q cx1 t1 = FlowState v cx1 t2
      where
        FlowState v _ t2 = unFlow f cx0 t1

withTrace :: String -> Flow s a -> Flow s a
withTrace msg = mutateTrace mutate restore
  where
    mutate t = if L.length (traceStack t) >= maxTraceDepth
      then Left "maximum trace depth exceeded. This may indicate an infinite loop"
      else Right $ t {traceStack = msg:(traceStack t)} -- augment the trace
    restore t0 t1 = t1 {traceStack = traceStack t0} -- reset the trace stack after execution
