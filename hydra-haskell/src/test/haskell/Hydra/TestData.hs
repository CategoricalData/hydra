module Hydra.TestData where

import Hydra.Kernel
import Hydra.Dsl.Terms
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.Map as M


makeMap :: [(String, Int)] -> Term
makeMap keyvals = Terms.map $ M.fromList $ ((\(k, v) -> (string k, int32 v)) <$> keyvals)
