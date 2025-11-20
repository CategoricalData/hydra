{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Test.TestTerms where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Module        as DModule
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import           Prelude hiding ((++))
import qualified Data.List               as L

import           Hydra.Dsl.Meta.Terms as MetaTerms


-- Type alias to avoid confusion with DSL Module
type HydraModule = Module

module_ :: HydraModule
module_ = Module (Namespace "hydra.test.testTerms") elements
    [TestTypes.module_]
    []
    (Just "Term definitions for the test suite")
  where
    elements = [
      el latlonRecordDef,
      el testDataArthurDef,
      el testElementArthurDef,
      el testElementFirstNameDef]

defineTerm :: String -> TTerm a -> TBinding a
defineTerm = definitionInModule module_

latlonRecordDef :: TBinding (Float -> Float -> Term)
latlonRecordDef = defineTerm "latlonRecord" $
  Phantoms.lambdas ["lat", "lon"] $ record (ref TestTypes.testTypeLatLonNameDef) [
    "lat">: float32Lift $ varPhantom "lat",
    "lon">: float32Lift $ varPhantom "lon"]

testDataArthurDef :: TBinding Term
testDataArthurDef = defineTerm "testDataArthur" $
  record (ref TestTypes.testTypePersonNameDef) [
    "firstName">: string "Arthur",
    "lastName">: string "Dent",
    "age">: int32 42]

testElementArthurDef :: TBinding Binding
testElementArthurDef = defineTerm "testElementArthur" $
  Core.binding
    (name "firstName")
    (ref testDataArthurDef)
    (Phantoms.just $ Core.typeScheme (Phantoms.list []) (Core.typeVariable $ ref TestTypes.testTypePersonNameDef))

testElementFirstNameDef :: TBinding Binding
testElementFirstNameDef = defineTerm "testElementFirstName" $
  Core.binding
    (name "firstName")
    (project (ref TestTypes.testTypePersonNameDef) (name "firstName"))
    (Phantoms.just $ Core.typeScheme (Phantoms.list [])
      (Core.typeFunction $ Core.functionType (Core.typeVariable $ ref TestTypes.testTypePersonNameDef) T.string))
