module Hydra.Sources.Test.All(
  module Hydra.Sources.Test.All,
) where

import Hydra.Kernel
import Hydra.Sources.Test.TestGraph
import Hydra.Sources.Test.TestSuite


testModules :: [Module]
testModules = [
  testGraphModule,
  testSuiteModule]
