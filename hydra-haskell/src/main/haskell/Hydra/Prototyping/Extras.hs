module Hydra.Prototyping.Extras (
  eitherToQualified,
  qualifiedToEither,
  module Hydra.Errors
  ) where

import Hydra.Errors

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

eitherToQualified :: Either String a -> Qualified a
eitherToQualified e = case e of
  Left msg -> Qualified Nothing [msg]
  Right x -> Qualified (Just x) []

qualifiedToEither :: Qualified a -> Either String a
qualifiedToEither (Qualified x m) = case x of
  Nothing -> Left $ L.head m
  Just x' -> Right x'
