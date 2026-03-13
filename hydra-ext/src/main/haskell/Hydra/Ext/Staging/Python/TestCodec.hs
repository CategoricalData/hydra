-- | Python test code generation codec for pytest-based generation tests
-- This thin wrapper delegates all codec logic to the generated module,
-- and only adds the TestGenerator (which involves IO infrastructure).

module Hydra.Ext.Staging.Python.TestCodec (
  pythonTestGenerator,
  generatePythonGenerationTests,
) where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Staging.Testing.Generation.Generate (TestGenerator(..), generateGenerationTestSuite)
import qualified Hydra.Ext.Python.TestCodec as Generated
import qualified Hydra.Ext.Python.Syntax as Py


-- | Python-specific test generator
pythonTestGenerator :: TestGenerator Py.DottedName
pythonTestGenerator = TestGenerator {
  testGenNamespacesForModule = Generated.namespacesForPythonModule,
  testGenCreateCodec = Generated.pythonTestCodec,
  testGenGenerateTestFile = Generated.generatePythonTestFile,
  testGenAggregatorFile = Nothing
}

-- | Main entry point for generating Python generation tests
generatePythonGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generatePythonGenerationTests = generateGenerationTestSuite pythonTestGenerator
