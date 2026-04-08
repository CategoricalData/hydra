-- | Haskell test code generation for HSpec-based generation tests.
-- This thin wrapper delegates test file generation to the generated module,
-- and only adds the TestGenerator (which involves IO infrastructure)
-- and the aggregator file generator (which uses System.FilePath).

module Hydra.Ext.Haskell.TestingIo where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Generation (showError, TestGenerator(..), generateGenerationTestSuite)
import Hydra.Test.Transform (addGenerationPrefix)
import Hydra.Ext.Haskell.Utils (namespacesForModule, sanitizeHaskellName)
import qualified Hydra.Ext.Haskell.Testing as Generated
import qualified Hydra.Ext.Haskell.Syntax as H
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.FilePath as FP
import Data.Char (toUpper)


-- | Haskell-specific test generator
haskellTestGenerator :: TestGenerator H.ModuleName
haskellTestGenerator = TestGenerator {
  testGeneratorNamespacesForModule = \m g -> do
    case namespacesForModule m emptyContext g of
      Left err -> Left (showError err)
      Right ns -> Right ns,
  testGeneratorGenerateTestFile = Generated.generateHaskellTestFile,
  testGeneratorAggregatorFile = Just generateHaskellAggregatorSpec
}

-- | Main entry point for generating Haskell generation tests
generateHaskellGenerationTests :: FilePath -> [Module] -> (Namespace -> Maybe TestGroup) -> IO Bool
generateHaskellGenerationTests = generateGenerationTestSuite haskellTestGenerator

-- | Generate an aggregator spec file that imports all generated test modules (Haskell/HSpec style)
generateHaskellAggregatorSpec :: FilePath -> [Module] -> (FilePath, String)
generateHaskellAggregatorSpec baseDir modules =
  let addSpecSuffix (Namespace ns) = Namespace (ns ++ "Spec")
      modulePaths = map (namespaceToModuleName . addSpecSuffix . addGenerationPrefix . moduleNamespace) modules
      imports = L.intercalate "\n" $ map (\m -> "import qualified " ++ m ++ " as " ++ sanitizeModuleName m) modulePaths
      specs = L.intercalate "\n    " $ map (\m -> sanitizeModuleName m ++ ".spec") modulePaths
      content = unlines [
        "-- Note: this is an automatically generated file. Do not edit.",
        "",
        "module Generation.Spec (spec) where",
        "",
        "import qualified Test.Hspec as H",
        imports,
        "",
        "spec :: H.Spec",
        "spec = do",
        "    " ++ specs
        ]
      filePath = FP.combine baseDir "Generation/Spec.hs"
  in (filePath, content)
  where
    sanitizeModuleName = map (\c -> if c == '.' then '_' else c)
    namespaceToModuleName (Namespace ns) =
      L.intercalate "." $ L.map capitalize (L.filter (not . null) $ Strings.splitOn "." ns)
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
