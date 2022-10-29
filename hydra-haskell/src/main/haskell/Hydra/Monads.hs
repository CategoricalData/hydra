module Hydra.Monads (
  module Hydra.Common,
  module Hydra.Core,
  module Hydra.Compute,
  module Hydra.Monads,
) where

import Hydra.Common
import Hydra.Core
import Hydra.Compute

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Control.Monad
import qualified System.IO as IO


type GraphFlow m = Flow (Context m)

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
    where
      pushError msg t = t {traceMessages = errorMsg:(traceMessages t)}
        where
          errorMsg = "Error: " ++ msg ++ " (" ++ L.intercalate " > " (L.reverse $ traceStack t) ++ ")"

emptyTrace :: Trace
emptyTrace = Trace [] [] M.empty

flowSucceeds :: s -> Flow s a -> Bool
flowSucceeds cx f = Y.isJust $ flowStateValue $ unFlow f cx emptyTrace

flowWarning :: String -> Flow s a -> Flow s a
flowWarning msg b = Flow u'
  where
    u' s0 t0 = FlowState v s1 t2
      where
        FlowState v s1 t1 = unFlow b s0 t0
        t2 = t1 {traceMessages = ("Warning: " ++ msg):(traceMessages t1)}

fromFlow :: s -> Flow s a -> a
fromFlow cx f = case flowStateValue (unFlow f cx emptyTrace) of
  Just x -> x

fromFlowIo :: s -> Flow s a -> IO.IO a
fromFlowIo cx f = case mv of
    Just v -> pure v
    Nothing -> fail $ traceSummary trace
  where
    FlowState mv _ trace = unFlow f cx emptyTrace

getState :: Flow s s
getState = Flow q
  where
    f = pure ()
    q s0 t0 = case v1 of
        Nothing -> FlowState Nothing s1 t1
        Just _ -> FlowState (Just s1) s1 t1
      where
        FlowState v1 s1 t1 = unFlow f s0 t0

putState :: s -> Flow s ()
putState cx = Flow q
  where
    q s0 t0 = FlowState v cx t1
      where
        FlowState v _ t1 = unFlow f s0 t0
        f = pure ()

traceSummary :: Trace -> String
traceSummary t = L.intercalate "\n" (L.nub $ traceMessages t)

unexpected :: (MonadFail m, Show a1) => String -> a1 -> m a2
unexpected cat obj = fail $ "expected " ++ cat ++ " but found: " ++ show obj

withState :: s1 -> Flow s1 a -> Flow s2 a
withState cx0 f = Flow q
  where
    q cx1 t1 = FlowState v cx1 t2
      where
        FlowState v _ t2 = unFlow f cx0 t1

withTrace :: String -> Flow s a -> Flow s a
withTrace msg f = Flow q
  where
    q s0 t0 = FlowState v s1 t3
      where
        FlowState v s1 t2 = unFlow f s0 t1
        t1 = t0 {traceStack = msg:(traceStack t0)}
        t3 = t2 {traceStack = traceStack t0}

withWarning :: String -> a -> Flow s a
withWarning msg x = flowWarning msg $ pure x
