
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

ns :: Namespace
ns = Namespace "hydra.test.testTerms"

module_ :: HydraModule
module_ = Module ns elements
    [TestTypes.ns]
    []
    (Just "Term definitions for the test suite")
  where
    elements = [
      Phantoms.toBinding latlonRecord,
      Phantoms.toBinding testDataArthur,
      Phantoms.toBinding testElementArthur,
      Phantoms.toBinding testElementFirstName]

defineTerm :: String -> TTerm a -> TBinding a
defineTerm = definitionInModule module_

latlonRecord :: TBinding (Float -> Float -> Term)
latlonRecord = defineTerm "latlonRecord" $
  Phantoms.lambdas ["lat", "lon"] $ record TestTypes.testTypeLatLonName [
    "lat">: float32Lift $ varPhantom "lat",
    "lon">: float32Lift $ varPhantom "lon"]

testDataArthur :: TBinding Term
testDataArthur = defineTerm "testDataArthur" $
  record TestTypes.testTypePersonName [
    "firstName">: string "Arthur",
    "lastName">: string "Dent",
    "age">: int32 42]

testElementArthur :: TBinding Binding
testElementArthur = defineTerm "testElementArthur" $
  Core.binding
    (name "firstName")
    testDataArthur
    (Phantoms.just $ Core.typeScheme (Phantoms.list ([] :: [TTerm Name])) (Core.typeVariable TestTypes.testTypePersonName))

testElementFirstName :: TBinding Binding
testElementFirstName = defineTerm "testElementFirstName" $
  Core.binding
    (name "firstName")
    (project TestTypes.testTypePersonName (name "firstName"))
    (Phantoms.just $ Core.typeScheme (Phantoms.list ([] :: [TTerm Name]))
      (Core.typeFunction $ Core.functionType (Core.typeVariable TestTypes.testTypePersonName) T.string))
