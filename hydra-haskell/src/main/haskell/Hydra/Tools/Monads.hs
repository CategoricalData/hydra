-- | Functions and type class implementations for working with Hydra's built-in Flow monad

module Hydra.Tools.Monads where

import Hydra.Compute
import Hydra.Errors
import Hydra.Monads
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Lib.Flows as Flows

import qualified Control.Monad as CM
import qualified System.IO as IO


fromEither :: Show e => (e -> String) -> Either e a -> Flow c a
fromEither printErr x = case x of
  Left e -> Flows.fail $ printErr e
  Right a -> return a

flowToIo :: s -> Flow s a -> IO.IO a
flowToIo cx f = case mv of
    Just v -> return v
    Nothing -> CM.fail $ traceSummary trace
  where
    FlowState mv _ trace = unFlow f cx emptyTrace

hydraEitherToHaskellEither :: Mantle.Either a b -> Either a b
hydraEitherToHaskellEither e = case e of
   Mantle.EitherLeft l -> Left l
   Mantle.EitherRight r -> Right r

fromMaybe :: String -> Maybe a -> Flow s a
fromMaybe message m = case m of
  Nothing -> Flows.fail message
  Just v -> return v
