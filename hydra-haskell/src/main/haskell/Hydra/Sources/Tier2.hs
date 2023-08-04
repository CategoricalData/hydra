{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2 where

import Hydra.Kernel
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Sources.Strip
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Flows as Flows
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Optionals as Optionals
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Prelude hiding ((++))
import qualified Data.Map as M
import qualified Data.Set as S


tier2Definition :: String -> Datum a -> Definition a
tier2Definition = definitionInModule hydraTier2Module

hydraTier2Module :: Module Kv
hydraTier2Module = Module (Namespace "hydra/tier2") elements [hydraGraphModule, hydraMantleModule, hydraComputeModule, hydraStripModule] $
    Just ("A module for miscellaneous tier-2 functions and constants.")
  where
   elements = [
     el getStateDef
     ]

getStateDef :: Definition (Flow s s)
getStateDef = tier2Definition "getState" $
  doc "Get the state of the current flow" $
  typed flowSS $
  wrap _Flow (lambda "s0" $ lambda "t0" $ (
    (lambda "v" $ lambda "s" $ lambda "t" $ (
      (matchOpt
        (Flows.flowState nothing (var "s") (var "t"))
        (constant (Flows.flowState (just $ var "s") (var "s") (var "t"))))
       @@ var "v"))
    @@ (Flows.flowStateValue @@ var "fs1") @@ (Flows.flowStateState @@ var "fs1") @@ (Flows.flowStateTrace @@ var "fs1"))
  `with` [
    "fs1">:
      typed (Types.apply (Types.apply (TypeVariable _FlowState) (Types.var "s")) Types.unit) $
      Flows.unFlow @@ (Flows.pure @@ unit) @@ var "s0" @@ var "t0"])
