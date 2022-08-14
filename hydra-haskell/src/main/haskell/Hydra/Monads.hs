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
--import Hydra.Lexical
--import Hydra.CoreDecoding
import Hydra.Evaluation

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad


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

qualifiedToResult :: Qualified m -> Result m
qualifiedToResult (Qualified x m) = case x of
  Nothing -> fail $ L.head m
  Just x' -> pure x'

resultToQualified :: Result m -> Qualified m
resultToQualified r = case r of
  ResultSuccess x -> pure x
  ResultFailure msg -> fail msg
