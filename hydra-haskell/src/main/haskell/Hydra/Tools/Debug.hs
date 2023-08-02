-- | Debugging utilities

module Hydra.Tools.Debug where

import Control.Exception

newtype DebugException = DebugException String deriving Show

instance Exception DebugException

debug = True :: Bool

throwDebugException :: String -> c
throwDebugException = throw . DebugException
