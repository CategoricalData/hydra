-- | Functions and type class implementations for working with Hydra's built-in Flow monad

module Hydra.Flows where

import Hydra.Constants
import Hydra.Core
import Hydra.Compute
import Hydra.Graph
import Hydra.Mantle
import qualified Hydra.Tier1 as Tier1

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
  fail msg = Flow $ \s t -> FlowState Nothing s (Tier1.pushError msg t)

fromFlowIo :: s -> Flow s a -> IO.IO a
fromFlowIo cx f = case mv of
    Just v -> pure v
    Nothing -> fail $ traceSummary trace
  where
    FlowState mv _ trace = unFlow f cx Tier1.emptyTrace

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

withTrace :: String -> Flow s a -> Flow s a
withTrace msg = Tier1.mutateTrace mutate restore
  where
    mutate t = if L.length (traceStack t) >= maxTraceDepth
      then EitherLeft "maximum trace depth exceeded. This may indicate an infinite loop"
      else EitherRight $ t {traceStack = msg:(traceStack t)} -- augment the trace
    restore t0 t1 = t1 {traceStack = traceStack t0} -- reset the trace stack after execution
