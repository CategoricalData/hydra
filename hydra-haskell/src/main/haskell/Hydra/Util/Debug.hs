module Hydra.Util.Debug where

import Control.Exception

newtype DebugException = DebugException String deriving Show

instance Exception DebugException

throwDebugException :: String -> c
throwDebugException = throw . DebugException
