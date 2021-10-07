module Hydra.Impl.Haskell.Extras (
  eitherToQualified,
  qualifiedToResult,
  module Hydra.Errors
  ) where

import Hydra.Errors
import Hydra.Prototyping.Steps

import qualified Data.List as L


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

eitherToQualified :: Result a -> Qualified a
eitherToQualified e = case e of
  ResultFailure msg -> Qualified Nothing [msg]
  ResultSuccess x -> Qualified (Just x) []

qualifiedToResult :: Qualified a -> Result a
qualifiedToResult (Qualified x m) = case x of
  Nothing -> fail $ L.head m
  Just x' -> pure x'
