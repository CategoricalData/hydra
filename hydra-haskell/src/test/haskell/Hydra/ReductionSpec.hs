module Hydra.ReductionSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Reduction
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Primitives

import Hydra.TestUtils
import Hydra.TestData

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Set as S


testBetaReduceTypeRecursively :: H.SpecWith ()
testBetaReduceTypeRecursively = do
  H.describe "Beta reduce types recursively" $ do
    
    H.it "Try non-application types" $ do
      H.shouldBe
        (reduce True Types.unit :: Type Meta)
        Types.unit
      H.shouldBe
        (reduce False latLonType :: Type Meta)
        latLonType

    H.it "Try simple application types" $ do
      H.shouldBe
        (reduce False app1)
        (Types.function Types.string Types.string)
      H.shouldBe
        (reduce False app2)
        latLonType
      H.shouldBe
        (reduce False app3)
        (Types.record [Types.field "foo" Types.unit])
        
    H.it "Try recursive application types" $ do
      H.shouldBe
        (reduce False app4)
        (Types.record [Types.field "f1" Types.int32, Types.field "f2" Types.int64])
        
    H.it "Distinguish between eager and lazy evaluation" $ do
      H.shouldBe
        (reduce False app5)
        (Types.record [Types.field "foo" app1])
      H.shouldBe
        (reduce True app5)
        (Types.record [Types.field "foo" $ Types.function Types.string Types.string])
  where
    ResultSuccess scx = schemaContext testContext
    reduce eager = betaReduceTypeRecursively eager scx 
    app1 = Types.apply (Types.lambda "t" $ Types.function (Types.variable "t") (Types.variable "t")) Types.string :: Type Meta
    app2 = Types.apply (Types.lambda "x" latLonType) Types.int32 :: Type Meta
    app3 = Types.apply (Types.lambda "a" $ Types.record [Types.field "foo" $ Types.variable "a"]) Types.unit :: Type Meta
    app4 = Types.apply (Types.apply (Types.lambda "x" $ Types.lambda "y" $ Types.record [
      Types.field "f1" $ Types.variable "x",
      Types.field "f2" $ Types.variable "y"]) Types.int32) Types.int64 :: Type Meta
    app5 = Types.apply (Types.lambda "a" $ Types.record [Types.field "foo" $ Types.variable "a"]) app1

spec :: H.Spec
spec = do
  testBetaReduceTypeRecursively
