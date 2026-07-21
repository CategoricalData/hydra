-- | Driver-level fail-loud tests for Hydra.PackageRouting (#560).
--
-- packages/hydra-build's Test/Routing.hs already exercises the GENERATED
-- module (Hydra.Build.Routing) directly for unrouted-namespace failures.
-- This spec closes the remaining gap: proving PackageRouting.hs's own
-- Either-to-error conversion wrapper (namespaceToPackageIn, groupByPackageIn)
-- actually propagates that fail-loud behavior rather than swallowing it.

module Hydra.PackageRoutingSpec where

import Hydra.Kernel (Module(..), ModuleName(..))
import Hydra.PackageRouting

import Control.Exception (ErrorCall(..), evaluate)
import Data.List (isPrefixOf, isInfixOf)
import qualified Test.Hspec as H


routedMap :: RoutingMap
routedMap = buildRoutingMap [("hydra-kernel", [ModuleName "hydra.only.module"])]

unroutedNs :: ModuleName
unroutedNs = ModuleName "hydra.totally.unrouted"

-- | Matches this driver's own error-message prefix (its contract), without
-- pinning down the generated module's Error Show instance (not this
-- driver's contract to test).
hasUnroutedPrefix :: String -> ErrorCall -> Bool
hasUnroutedPrefix ns (ErrorCall msg) =
  ("unrouted module: " ++ ns) `isPrefixOf` msg

spec :: H.Spec
spec = do
  H.describe "namespaceToPackageIn (#560)" $ do

    H.it "crashes on an unrouted namespace instead of silently falling back" $
      evaluate (namespaceToPackageIn routedMap unroutedNs)
        `H.shouldThrow` hasUnroutedPrefix "hydra.totally.unrouted"

  H.describe "groupByPackageIn (#560)" $ do

    H.it "crashes while grouping if any module is unrouted" $
      evaluate (groupByPackageIn routedMap [Module unroutedNs Nothing [] []])
        `H.shouldThrow` (\(ErrorCall msg) -> "unrouted module encountered while grouping" `isInfixOf` msg)
