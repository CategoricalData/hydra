{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Test.TestTerms where

import Hydra.Kernel
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Module        as DModule
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types        as T
import           Hydra.Sources.Test.TestTypes
import           Prelude hiding ((++))
import qualified Data.List               as L

import           Hydra.Dsl.Meta.Terms as MetaTerms


-- Type alias to avoid confusion with DSL Module
type HydraModule = Module

testTermsDefinition :: String -> TTerm a -> TBinding a
testTermsDefinition = definitionInModule testTermsModule

testTermsModule :: HydraModule
testTermsModule = Module (Namespace "hydra.test.testTerms") elements
    [testTypesModule]
    []
    (Just "Term definitions for the test suite")
  where
    elements = [
      el latlonRecordDef,
      el testDataArthurDef,
      el testElementArthurDef,
      el testElementFirstNameDef]

latlonRecordDef :: TBinding (Float -> Float -> Term)
latlonRecordDef = testTermsDefinition "latlonRecord" $
  Phantoms.lambdas ["lat", "lon"] $ record (ref testTypeLatLonNameDef) [
    "lat">: float32Lift $ varPhantom "lat",
    "lon">: float32Lift $ varPhantom "lon"]

testDataArthurDef :: TBinding Term
testDataArthurDef = testTermsDefinition "testDataArthur" $
  record (ref testTypePersonNameDef) [
    "firstName">: string "Arthur",
    "lastName">: string "Dent",
    "age">: int32 42]

testElementArthurDef :: TBinding Binding
testElementArthurDef = testTermsDefinition "testElementArthur" $
  Core.binding
    (name "firstName")
    (ref testDataArthurDef)
    (Phantoms.just $ Core.typeScheme (Phantoms.list []) (Core.typeVariable $ ref testTypePersonNameDef))

testElementFirstNameDef :: TBinding Binding
testElementFirstNameDef = testTermsDefinition "testElementFirstName" $
  Core.binding
    (name "firstName")
    (project (ref testTypePersonNameDef) (name "firstName"))
    (Phantoms.just $ Core.typeScheme (Phantoms.list [])
      (Core.typeFunction $ Core.functionType (Core.typeVariable $ ref testTypePersonNameDef) T.string))
