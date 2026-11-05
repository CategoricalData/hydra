-- | Regression test for #649: 'generateSourceFiles' used to combine its type-pass and
-- term-pass results with an unkeyed concatenation. A module with both type and term
-- definitions produces one result entry per pass, both targeting the same per-module
-- file path (true of module-per-file coders such as Python's and Haskell's own); the
-- unkeyed combination left both entries in the result list, and every driver that
-- writes each entry to its path in list order (e.g. 'Hydra.Generation.writePair') had
-- the term-pass entry silently overwrite the type-pass entry, dropping the module's
-- types from the generated file.
--
-- This test drives 'Hydra.Core.Codegen.generateSourceFiles' directly with the real
-- Python coder ('moduleToPython') over a module containing one type definition and one
-- term definition, and asserts that the result contains exactly one entry for the
-- module's path, whose content includes both definitions.

module Hydra.GenerateSourceFilesSpec where

import Hydra.Kernel
import Hydra.Core.Overlay.Haskell.Bootstrap (bootstrapGraph)
import qualified Hydra.Core.Codegen as CodeGeneration
import Hydra.Generation (showError)
import Hydra.Python.Coder (moduleToPython)
import Hydra.Python.Language (pythonLanguage)
import Hydra.Core.Packaging (Module(..), ModuleName(..), Definition(..), TypeDefinition(..), TermDefinition(..))
import Hydra.Core.Model (Name(..), Type(..), TypeScheme(..), FieldType(..), LiteralType(..), Literal(..), Term(..))
import Hydra.Core.Typing (TermSignature(..), Result(..))
import Hydra.TestUtils (testContext)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL


-- A minimal module with one type definition and one term definition, so that
-- 'generateSourceFiles' produces both a schema-pass and a term-pass result for it.
testModule :: Module
testModule = Module (ModuleName "test.GenerateSourceFiles") Nothing []
  [ DefinitionType $ TypeDefinition (Name "test.GenerateSourceFiles.Rec") Nothing
      (TypeScheme [] (TypeRecord [FieldType (Name "value") (TypeLiteral LiteralTypeString)]) M.empty)
  , DefinitionTerm $ TermDefinition (Name "test.GenerateSourceFiles.greet") Nothing
      (Just (TermSignature [] [] (Result Nothing (TypeLiteral LiteralTypeString))))
      (TermLiteral (LiteralString "hello"))
  ]

generate :: Either Error [(String, String)]
generate = CodeGeneration.generateSourceFiles (moduleToPython S.empty) pythonLanguage False
  bootstrapGraph [] [testModule] testContext

spec :: H.Spec
spec = do
  H.describe "generateSourceFiles merges same-path type and term results (#649)" $ do
    H.it "produces exactly one file for a module with both types and terms" $ do
      case generate of
        Left err -> HL.assertFailure (showError err)
        Right files -> L.length files `H.shouldBe` 1

    H.it "the single file contains both the type and the term definition" $ do
      case generate of
        Left err -> HL.assertFailure (showError err)
        Right [(_, content)] -> do
          L.isInfixOf "Rec" content `H.shouldBe` True
          L.isInfixOf "greet" content `H.shouldBe` True
        Right files -> HL.assertFailure $
          "expected exactly one result file, got " ++ show (L.length files) ++ ": " ++ show (map fst files)
