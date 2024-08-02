module Hydra.Tier1Spec where

import Hydra.Kernel

import Hydra.TestUtils
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


checkFoldOverTerm :: H.SpecWith ()
checkFoldOverTerm = do
  H.describe "Test foldOverTerm" $ do
    H.describe "Pre-order" $ do
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPre node1)
          ["a"]
      H.it "test #2" $
        H.shouldBe
          (traverse TraversalOrderPre node2)
          ["a", "b", "c", "d"]
    H.describe "Post-order" $ do
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPost node1)
          ["a"]
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPost node2)
          ["b", "d", "c", "a"]
  where
    node label children = Terms.pair (Terms.string label) (Terms.list children)
    labelOf term = case term of
      TermProduct [TermLiteral (LiteralString label), _] -> Just label
      _ -> Nothing
    traverse :: TraversalOrder -> Term -> [String]
    traverse order = Y.catMaybes . foldOverTerm order (\l t -> l ++ [labelOf t]) []
    node1 = node "a" []
    node2 = node "a" [node "b" [], node "c" [node "d" []]]

spec :: H.Spec
spec = do
  checkFoldOverTerm
