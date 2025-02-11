-- | A module defining the graph used in the test suite.

module Hydra.Test.TestGraph where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

testTypeLatLonName :: Core.Name
testTypeLatLonName = (Core.Name "LatLon")

testTypeLatLonPolyName :: Core.Name
testTypeLatLonPolyName = (Core.Name "LatLonPoly")

latlonRecord :: (Float -> Float -> Core.Term)
latlonRecord lat lon = (Core.TermRecord (Core.Record {
  Core.recordTypeName = testTypeLatLonName,
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lat"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 lat)))},
    Core.Field {
      Core.fieldName = (Core.Name "lon"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 lon)))}]}))