module Hydra.Monads (
  module Hydra.Monads,
  module Hydra.Common,
  module Hydra.Errors,
  module Hydra.Evaluation,
) where

import Hydra.Common
import Hydra.Core
import Hydra.Errors
import Hydra.Graph
import Hydra.Evaluation

import qualified Data.List as L
import qualified Data.Map as M
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
      
instance Functor Qualified where
  fmap f (Qualified x msgs) = Qualified (fmap f x) msgs
instance Applicative Qualified where
  pure x = Qualified (Just x) []
  Qualified f mf <*> Qualified x mx = Qualified (f <*> x) $ mf <> mx
instance Monad Qualified where
  Qualified x m >>= f = case x of
    Nothing -> Qualified Nothing m
    Just x' -> Qualified fx $ m2 <> m
      where Qualified fx m2 = f x'
instance MonadFail Qualified where
  fail msg = Qualified Nothing [msg]

instance Functor Result where
  fmap f r = case r of
    ResultFailure msg -> ResultFailure msg
    ResultSuccess x -> ResultSuccess $ f x 
instance Applicative Result where
  pure = ResultSuccess
  rf <*> rx = case (rf, rx) of
    (_, ResultFailure msg) -> ResultFailure msg
    (ResultFailure msg, _) -> ResultFailure msg
    (ResultSuccess f', ResultSuccess x') -> ResultSuccess $ f' x'
instance Monad Result where
  r >>= f = case r of
    ResultFailure msg -> ResultFailure msg 
    ResultSuccess x -> f x
instance MonadFail Result where
  fail = ResultFailure

--debug :: String -> Result m -> Result m
--debug msg r = case r of
--  ResultSuccess _ -> r
--  ResultFailure msg1 -> ResultFailure $ "failure[" ++ msg ++ "]: " ++ msg1

eitherToQualified :: Result m -> Qualified m
eitherToQualified e = case e of
  ResultFailure msg -> Qualified Nothing [msg]
  ResultSuccess x -> Qualified (Just x) []

emptyTrace :: Trace
emptyTrace = Trace [] []

flowWarning :: String -> Flow s a -> Flow s a
flowWarning msg b = Flow u'
  where
    u' s0 t0 = FlowWrapper v s1 t2
      where
        FlowWrapper v s1 t1 = unFlow b s0 t0
        t2 = t1 {traceMessages = (msg:(traceStack t1)):(traceMessages t1)}

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

qualifiedToResult :: Qualified m -> Result m
qualifiedToResult (Qualified x m) = case x of
  Nothing -> fail $ L.head m
  Just x' -> pure x'

resultToQualified :: Result m -> Qualified m
resultToQualified r = case r of
  ResultSuccess x -> pure x
  ResultFailure msg -> fail msg
