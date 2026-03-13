-- | Java test code generation codec for JUnit-based generation tests
-- This thin wrapper delegates all codec logic to the generated module,
-- and only adds the TestGenerator (which involves IO infrastructure).

module Hydra.Ext.Staging.Java.TestCodec (
  javaTestGenerator,
  generateJavaGenerationTests,
) where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Staging.Testing.Generation.Generate (TestGenerator(..), generateGenerationTestSuite)
import qualified Hydra.Ext.Java.TestCodec as Generated

import qualified Data.Map as M


-- | Java-specific test generator
javaTestGenerator :: TestGenerator ()
javaTestGenerator = TestGenerator {
  testGenNamespacesForModule = \_ _ -> Right $ Namespaces (Namespace "", ()) M.empty,
  testGenCreateCodec = \_ -> Generated.javaTestCodec,
  testGenGenerateTestFile = Generated.generateJavaTestFile,
  testGenAggregatorFile = Nothing
}

-- | Main entry point for generating Java generation tests
generateJavaGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateJavaGenerationTests = generateGenerationTestSuite javaTestGenerator
