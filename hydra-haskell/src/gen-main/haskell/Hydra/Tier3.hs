-- | A module for miscellaneous tier-3 functions and constants.

module Hydra.Tier3 where

import Hydra.Compute
import Hydra.Lib.Io
import qualified Hydra.Tier1 as Tier1

import qualified Data.List as L
import qualified Data.Map as M


traceSummary :: Trace -> String
traceSummary t = L.intercalate "\n" (messageLines ++ keyvalLines)
  where
    messageLines = L.nub $ traceMessages t
    keyvalLines = if M.null (traceOther t)
        then []
        else "key/value pairs:":(toLine <$> M.toList (traceOther t))
    toLine (k, v) = "\t" ++ k ++ ": " ++ showTerm v
