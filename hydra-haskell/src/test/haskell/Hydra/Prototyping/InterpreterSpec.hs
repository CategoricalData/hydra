module Hydra.Prototyping.InterpreterSpec where

import Hydra.Core
import Hydra.Graph
import Hydra.Prototyping.Helpers
import Hydra.Prototyping.Interpreter
import Hydra.Prototyping.Primitives

import Hydra.ArbitraryCore
import Hydra.TestGraph

import Test.Hspec
import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.Set as S
import qualified Test.QuickCheck as QC


testsForAtomicValues = do
  describe "Tests for atomic values" $ do
    
    it "Atomic terms have no free variables" $
      QC.property $ \av -> termIsClosed (TermAtomic av)

    it "Atomic terms are fully reduced; check using a dedicated function" $
      QC.property $ \av -> termIsValue testStrategy (TermAtomic av)

    it "Atomic terms are fully reduced; check by trying to reduce them" $
      QC.property $ \av -> evaluate testContext (TermAtomic av) == (Right (TermAtomic av))

    it "Atomic terms cannot be applied" $
      QC.property $ \av term -> E.isLeft (evaluate testContext $ apply (TermAtomic av) term)

testsForPrimitiveFunctions = do
  describe "Tests for primitive functions" $ do
    
    it "Example primitives have the expected arity" $ do
      (primitiveFunctionArity <$> lookupPrimitiveFunction testContext "toUpper") `shouldBe` (Just 1)
      (primitiveFunctionArity <$> lookupPrimitiveFunction testContext "concat") `shouldBe` (Just 2)
    
    it "Simple applications of a unary function succeed" $
      QC.property $ \s -> 
        (evaluate testContext (apply (function "toUpper") $ stringTerm s))
        == (Right $ stringTerm $ fmap C.toUpper s)
        
    it "Simple applications of a binary function succeed" $
      QC.property $ \s1 s2 ->
        (evaluate testContext (apply (apply (function "concat") $ stringTerm s1) $ stringTerm s2))
        == (Right $ stringTerm $ s1 ++ s2)

    it "Incomplete application of a primitive function leaves the term unchanged" $ 
      QC.property $ \s1 ->
        (evaluate testContext (apply (function "concat") $ stringTerm s1))
        == (Right $ apply (function "concat") $ stringTerm s1)

    it "Extra arguments to a primitive function cause failure" $
      QC.property $ \s1 s2 ->
        E.isLeft (evaluate testContext (apply (apply (function "toUpper") $ stringTerm s1) $ stringTerm s2))

spec :: Spec
spec = do
  testsForAtomicValues
  testsForPrimitiveFunctions
