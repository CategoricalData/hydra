module Hydra.Sources.Tier3.Test.TestGraph where

import           Hydra.Dsl.Base          as Base
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier2.All
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


testGraphDefinition :: String -> TTerm a -> TElement a
testGraphDefinition = definitionInModule testGraphModule

testGraphModule :: Module
testGraphModule = Module (Namespace "hydra/test/testGraph") elements [] [hydraGraphModule] $
    Just ("A module defining the graph used in the test suite.")
  where
   elements = [
     el testTypeLatLonNameDef,
     el testTypeLatLonPolyNameDef,
     el latlonRecordDef]

testTypeLatLonNameDef :: TElement Name
testTypeLatLonNameDef = testGraphDefinition "testTypeLatLonName" $
  Core.name $ Name "LatLon"

testTypeLatLonPolyNameDef :: TElement Name
testTypeLatLonPolyNameDef = testGraphDefinition "testTypeLatLonPolyName" $
  Core.name $ Name "LatLonPoly"

latlonRecordDef :: TElement (Float -> Float -> Term)
latlonRecordDef = testGraphDefinition "latlonRecord" $
  functionN [tFloat32, tFloat32, termT] $
  lambdas ["lat", "lon"] $ Core.termRecord $ Core.record (ref testTypeLatLonNameDef) $ Core.list [
    Core.field (Core.name $ Name "lat") $ Core.float32 $ Core.var "lat",
    Core.field (Core.name $ Name "lon") $ Core.float32 $ Core.var "lon"]





