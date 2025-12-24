-- Note: this is an automatically generated file. Do not edit.

-- | Term definitions for the test suite

module Hydra.Test.TestTerms where

import qualified Hydra.Core as Core
import qualified Hydra.Test.TestTypes as TestTypes
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

latlonRecord :: (Float -> Float -> Core.Term)
latlonRecord lat lon = (Core.TermRecord (Core.Record {
  Core.recordTypeName = TestTypes.testTypeLatLonName,
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lat"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 lat)))},
    Core.Field {
      Core.fieldName = (Core.Name "lon"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 lon)))}]}))

testDataArthur :: Core.Term
testDataArthur = (Core.TermRecord (Core.Record {
  Core.recordTypeName = TestTypes.testTypePersonName,
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "firstName"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Arthur"))},
    Core.Field {
      Core.fieldName = (Core.Name "lastName"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Dent"))},
    Core.Field {
      Core.fieldName = (Core.Name "age"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}]}))

testElementArthur :: Core.Binding
testElementArthur = Core.Binding {
  Core.bindingName = (Core.Name "firstName"),
  Core.bindingTerm = testDataArthur,
  Core.bindingType = (Just (Core.TypeScheme {
    Core.typeSchemeVariables = [],
    Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypePersonName),
    Core.typeSchemeConstraints = Nothing}))}

testElementFirstName :: Core.Binding
testElementFirstName = Core.Binding {
  Core.bindingName = (Core.Name "firstName"),
  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = TestTypes.testTypePersonName,
    Core.projectionField = (Core.Name "firstName")})))),
  Core.bindingType = (Just (Core.TypeScheme {
    Core.typeSchemeVariables = [],
    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypePersonName),
      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
    Core.typeSchemeConstraints = Nothing}))}
