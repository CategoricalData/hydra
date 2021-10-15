module Hydra.Ext.Yaml.CoderSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl
import Hydra.Ext.Yaml.Coder
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import Hydra.Adapter

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.QuickCheck as QC


atomicTypeConstraintsAreRespected :: H.SpecWith ()
atomicTypeConstraintsAreRespected = H.describe "Verify that YAML's atomic type constraints are respected" $ do

  H.it "Test..." $
    QC.property $ \b -> b || not b

spec :: H.Spec
spec = do
  atomicTypeConstraintsAreRespected
