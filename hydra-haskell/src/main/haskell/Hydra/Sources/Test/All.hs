module Hydra.Sources.Test.All where

import Hydra.Kernel

import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes

import qualified Data.Map as M


testModules :: [Module]
testModules = [
  TestGraph.module_,
  TestTerms.module_,
  TestTypes.module_]
  ++ moduleAndDependencies TestSuite.module_

moduleAndDependencies :: Module -> [Module]
moduleAndDependencies modl = M.elems $ add M.empty modl
  where
    add m md = M.insert (moduleNamespace md) md m `M.union` M.unions (fmap (add m) (moduleTermDependencies md))
