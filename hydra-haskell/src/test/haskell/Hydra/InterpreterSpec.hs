module Hydra.InterpreterSpec where

import Hydra.Basics
import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Interpreter
import Hydra.Primitives
import Hydra.Steps

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Data.Char as C
import qualified Data.List as L
import qualified Test.QuickCheck as QC


checkLiterals :: H.SpecWith ()
checkLiterals = do
  H.describe "Tests for atomic values" $ do

    H.it "Atomic terms have no free variables" $
      QC.property $ \av -> termIsClosed (atomic av :: Term Meta)

    H.it "Atomic terms are fully reduced; check using a dedicated function" $
      QC.property $ \av -> termIsValue testStrategy (atomic av :: Term Meta)

    H.it "Atomic terms are fully reduced; check by trying to reduce them" $
      QC.property $ \av ->
        H.shouldBe
          (eval (atomic av))
          (pure (atomic av :: Term Meta))

    H.it "Atomic terms cannot be applied" $
      QC.property $ \av (TypedTerm _ term) -> isFailure (eval $ apply (atomic av) term)

checkMonomorphicPrimitives :: H.SpecWith ()
checkMonomorphicPrimitives = do
  H.describe "Tests for monomorphic primitive functions" $ do

    H.it "Example primitives have the expected arity" $ do
      H.shouldBe
        (primitiveFunctionArity <$> lookupPrimitiveFunction testContext _strings_toUpper)
        (Just 1)
      H.shouldBe
        (primitiveFunctionArity <$> lookupPrimitiveFunction testContext _strings_splitOn)
        (Just 2)

    H.it "Simple applications of a unary function succeed" $
      QC.property $ \s ->
        H.shouldBe
          (eval (apply (primitive _strings_toUpper) $ string s))
          (pure (string $ fmap C.toUpper s))

    H.it "Simple applications of a binary function succeed" $
      QC.property $ \i1 i2 ->
        H.shouldBe
          (eval (apply (apply (primitive _math_add) $ int32 i1) $ int32 i2))
          (pure (int32 $ i1 + i2))

    H.it "Incomplete application of a primitive function leaves the term unchanged" $
      QC.property $ \s1 ->
        H.shouldBe
          (eval (apply (primitive _strings_splitOn) $ string s1))
          (pure (apply (primitive _strings_splitOn) $ string s1))

    H.it "Extra arguments to a primitive function cause failure" $
      QC.property $ \s1 s2 ->
        isFailure (eval (apply (apply (primitive _strings_toUpper) $ string s1) $ string s2))

checkPolymorphicPrimitives :: H.SpecWith ()
checkPolymorphicPrimitives = do
  H.describe "Tests for polymorphic primitive functions" $ do

    H.it "Test polymorphic list length" $ do
      QC.property $ \l ->
        H.shouldBe
          (eval (apply (primitive _lists_length) $ list l))
          (pure (int32 $ L.length l))

spec :: H.Spec
spec = do
  checkLiterals
  checkMonomorphicPrimitives
  checkPolymorphicPrimitives
  
eval :: Term Meta -> Result (Term Meta)
eval = evaluate testContext
