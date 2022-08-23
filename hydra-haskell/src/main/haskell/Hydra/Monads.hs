module Hydra.Monads (
  module Hydra.Common,
  module Hydra.Core,
  module Hydra.Evaluation,
  module Hydra.Monads,
) where

import Hydra.Common
import Hydra.Core
import Hydra.Evaluation

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Control.Monad


type GraphFlow m = Flow (Context m)

instance Functor (Flow s) where
  fmap = liftM
instance Applicative (Flow s) where
  pure = return
  (<*>) = ap
instance Monad (Flow s) where
  return x = Flow $ \s t -> FlowWrapper (Just x) s t
  p >>= k = Flow q'
    where
      q' s0 t0 = FlowWrapper y s2 t2
        where
          FlowWrapper x s1 t1 = unFlow p s0 t0
          FlowWrapper y s2 t2 = case x of
            Just x' -> unFlow (k x') s1 t1
            Nothing -> FlowWrapper Nothing s1 t1
instance MonadFail (Flow s) where
  fail msg = Flow $ \s t -> FlowWrapper Nothing s (pushError msg t)
    where
      pushError msg t = t {traceStack = ("Error: " ++ msg):(traceStack t)}

emptyTrace :: Trace
emptyTrace = Trace [] []

flowSucceeds :: s -> Flow s a -> Bool
flowSucceeds cx f = Y.isJust $ flowWrapperValue $ unFlow f cx emptyTrace

flowWarning :: String -> Flow s a -> Flow s a
flowWarning msg b = Flow u'
  where
    u' s0 t0 = FlowWrapper v s1 t2
      where
        FlowWrapper v s1 t1 = unFlow b s0 t0
        t2 = t1 {traceMessages = (msg:(traceStack t1)):(traceMessages t1)}

fromFlow :: s -> Flow s a -> a
fromFlow cx f = case flowWrapperValue (unFlow f cx emptyTrace) of
  Just x -> x
     
getState :: Flow s s
getState = Flow q
  where
    f = pure ()
    q s0 t0 = case v1 of
        Nothing -> FlowWrapper Nothing s1 t1
        Just _ -> FlowWrapper (Just s1) s1 t1
      where
        FlowWrapper v1 s1 t1 = unFlow f s0 t0

pushTrc :: String -> Trace -> Trace
pushTrc msg t = t {traceStack = msg:(traceStack t)}

putState :: s -> Flow s ()
putState cx = Flow q
  where
    q s0 t0 = FlowWrapper v cx t1
      where
        FlowWrapper v _ t1 = unFlow f s0 t0
        f = pure ()

traceSummary :: Trace -> String
traceSummary (Trace stack messages) = L.intercalate "\n" (errorLine:warningLines)
  where
    errorLine = "Error: " ++ printStack stack
    warningLine s = "\t" ++ printStack s
    warningLines = if L.null messages
      then []
      else "Warnings:":(warningLine <$> messages)
    printStack s = L.intercalate " > " $ L.reverse s

unexpected :: (MonadFail m, Show a1) => String -> a1 -> m a2
unexpected cat obj = fail $ "expected " ++ cat ++ " but found: " ++ show obj

withState :: s1 -> Flow s1 a -> Flow s2 a
withState cx0 f = Flow q
  where
    q cx1 t1 = FlowWrapper v cx1 t2
      where
        FlowWrapper v _ t2 = unFlow f cx0 t1

withWarning :: String -> a -> Flow s a
withWarning msg x = flowWarning msg $ pure x
