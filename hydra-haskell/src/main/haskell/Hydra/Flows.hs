-- | Functions and type class implementations for working with Hydra's built-in Flow monad

module Hydra.Flows where

import Hydra.Kernel
import qualified Hydra.Lib.Flows as Flows

import qualified Control.Monad as CM
import qualified System.IO as IO


fromEither :: Show e => Either e a -> Flow c a
fromEither x = case x of
  Left e -> Flows.fail $ show e
  Right a -> return a

fromFlowIo :: s -> Flow s a -> IO.IO a
fromFlowIo cx f = case mv of
    Just v -> return v
    Nothing -> CM.fail $ traceSummary trace
  where
    FlowState mv _ trace = unFlow f cx emptyTrace
