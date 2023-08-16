{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3 where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Flows as Flows
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Io as Io
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Logic as Logic
import Hydra.Dsl.Lib.Optionals as Optionals
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Prelude hiding ((++))
import qualified Data.Map as M
import qualified Data.Set as S


tier3Definition :: String -> Datum a -> Definition a
tier3Definition = definitionInModule hydraTier3Module

hydraTier3Module :: Module Kv
hydraTier3Module = Module (Namespace "hydra/tier3") elements [hydraCoreModule] $
    Just ("A module for miscellaneous tier-3 functions and constants.")
  where
   elements = [
--     traceSummaryDef
     ]
