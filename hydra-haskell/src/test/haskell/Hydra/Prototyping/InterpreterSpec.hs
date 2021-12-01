module Hydra.Prototyping.InterpreterSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Prototyping.Interpreter
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.Steps

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Data.Char as C
import qualified Test.QuickCheck as QC


testEvaluate :: Term Meta -> Result (Term Meta)
testEvaluate = evaluate testContext

testsForLiterals :: H.SpecWith ()
testsForLiterals = do
  H.describe "Tests for atomic values" $ do

    H.it "Atomic terms have no free variables" $
      QC.property $ \av -> termIsClosed (atomic av :: Term Meta)

    H.it "Atomic terms are fully reduced; check using a dedicated function" $
      QC.property $ \av -> termIsValue testStrategy (atomic av :: Term Meta)

    H.it "Atomic terms are fully reduced; check by trying to reduce them" $
      QC.property $ \av -> testEvaluate (atomic av) == pure (atomic av :: Term Meta)

    H.it "Atomic terms cannot be applied" $
      QC.property $ \av (TypedTerm _ term) -> isFailure (testEvaluate $ apply (atomic av) term)

testsForPrimitiveFunctions :: H.SpecWith ()
testsForPrimitiveFunctions = do
  H.describe "Tests for primitive functions" $ do

    H.it "Example primitives have the expected arity" $ do
      (primitiveFunctionArity <$> lookupPrimitiveFunction testContext (stringsFunc "toUpper"))
        `H.shouldBe` Just 1
      (primitiveFunctionArity <$> lookupPrimitiveFunction testContext (stringsFunc "cat"))
        `H.shouldBe` Just 2

    H.it "Simple applications of a unary function succeed" $
      QC.property $ \s ->
        testEvaluate (apply (func "toUpper") $ stringTerm s)
        == pure (stringTerm $ fmap C.toUpper s)

    H.it "Simple applications of a binary function succeed" $
      QC.property $ \s1 s2 ->
        testEvaluate (apply (apply (func "cat") $ stringTerm s1) $ stringTerm s2)
        == pure (stringTerm $ s1 ++ s2)

    H.it "Incomplete application of a primitive function leaves the term unchanged" $
      QC.property $ \s1 ->
        testEvaluate (apply (func "cat") $ stringTerm s1)
        == pure (apply (func "cat") $ stringTerm s1)

    H.it "Extra arguments to a primitive function cause failure" $
      QC.property $ \s1 s2 ->
        isFailure (testEvaluate (apply (apply (func "toUpper") $ stringTerm s1) $ stringTerm s2))

func :: String -> Term Meta
func = primitive . stringsFunc

spec :: H.Spec
spec = do
  testsForLiterals
  testsForPrimitiveFunctions
