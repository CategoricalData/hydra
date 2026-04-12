-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.geojson.model

module Hydra.Dsl.Ext.Org.Geojson.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Geojson.Model as Model
import qualified Hydra.Json.Model as Model_
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

boundingBox :: Phantoms.TTerm [Model.CoordinateRange] -> Phantoms.TTerm Model.BoundingBox
boundingBox x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.geojson.model.BoundingBox"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

coordinateRange :: Phantoms.TTerm Double -> Phantoms.TTerm Double -> Phantoms.TTerm Model.CoordinateRange
coordinateRange min max =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.CoordinateRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm max)}]}))

coordinateRangeMax :: Phantoms.TTerm Model.CoordinateRange -> Phantoms.TTerm Double
coordinateRangeMax x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.CoordinateRange"),
        Core.projectionField = (Core.Name "max")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coordinateRangeMin :: Phantoms.TTerm Model.CoordinateRange -> Phantoms.TTerm Double
coordinateRangeMin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.CoordinateRange"),
        Core.projectionField = (Core.Name "min")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coordinateRangeWithMax :: Phantoms.TTerm Model.CoordinateRange -> Phantoms.TTerm Double -> Phantoms.TTerm Model.CoordinateRange
coordinateRangeWithMax original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.CoordinateRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.CoordinateRange"),
              Core.projectionField = (Core.Name "min")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

coordinateRangeWithMin :: Phantoms.TTerm Model.CoordinateRange -> Phantoms.TTerm Double -> Phantoms.TTerm Model.CoordinateRange
coordinateRangeWithMin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.CoordinateRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.CoordinateRange"),
              Core.projectionField = (Core.Name "max")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

feature :: Phantoms.TTerm (Maybe Model.Geometry) -> Phantoms.TTerm (Maybe (M.Map String Model_.Value)) -> Phantoms.TTerm (Maybe Model.Id) -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.Feature
feature geometry properties id bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Phantoms.unTTerm geometry)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

featureBbox :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe Model.BoundingBox)
featureBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureCollection :: Phantoms.TTerm [Model.Feature] -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.FeatureCollection
featureCollection features bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.FeatureCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "features"),
          Core.fieldTerm = (Phantoms.unTTerm features)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

featureCollectionBbox :: Phantoms.TTerm Model.FeatureCollection -> Phantoms.TTerm (Maybe Model.BoundingBox)
featureCollectionBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.FeatureCollection"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureCollectionFeatures :: Phantoms.TTerm Model.FeatureCollection -> Phantoms.TTerm [Model.Feature]
featureCollectionFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.FeatureCollection"),
        Core.projectionField = (Core.Name "features")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureCollectionWithBbox :: Phantoms.TTerm Model.FeatureCollection -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.FeatureCollection
featureCollectionWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.FeatureCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "features"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.FeatureCollection"),
              Core.projectionField = (Core.Name "features")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

featureCollectionWithFeatures :: Phantoms.TTerm Model.FeatureCollection -> Phantoms.TTerm [Model.Feature] -> Phantoms.TTerm Model.FeatureCollection
featureCollectionWithFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.FeatureCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "features"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.FeatureCollection"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featureGeometry :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe Model.Geometry)
featureGeometry x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
        Core.projectionField = (Core.Name "geometry")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureId :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe Model.Id)
featureId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureProperties :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe (M.Map String Model_.Value))
featureProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureWithBbox :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.Feature
featureWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "geometry")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

featureWithGeometry :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe Model.Geometry) -> Phantoms.TTerm Model.Feature
featureWithGeometry original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featureWithId :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe Model.Id) -> Phantoms.TTerm Model.Feature
featureWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "geometry")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featureWithProperties :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm (Maybe (M.Map String Model_.Value)) -> Phantoms.TTerm Model.Feature
featureWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "geometry")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Feature"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

geometryCollection :: Phantoms.TTerm [Model.Geometry] -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.GeometryCollection
geometryCollection geometries bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.GeometryCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometries"),
          Core.fieldTerm = (Phantoms.unTTerm geometries)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

geometryCollectionBbox :: Phantoms.TTerm Model.GeometryCollection -> Phantoms.TTerm (Maybe Model.BoundingBox)
geometryCollectionBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.GeometryCollection"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

geometryCollectionGeometries :: Phantoms.TTerm Model.GeometryCollection -> Phantoms.TTerm [Model.Geometry]
geometryCollectionGeometries x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.GeometryCollection"),
        Core.projectionField = (Core.Name "geometries")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

geometryCollectionWithBbox :: Phantoms.TTerm Model.GeometryCollection -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.GeometryCollection
geometryCollectionWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.GeometryCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometries"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.GeometryCollection"),
              Core.projectionField = (Core.Name "geometries")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

geometryCollectionWithGeometries :: Phantoms.TTerm Model.GeometryCollection -> Phantoms.TTerm [Model.Geometry] -> Phantoms.TTerm Model.GeometryCollection
geometryCollectionWithGeometries original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.GeometryCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometries"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.GeometryCollection"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

geometryGeometryCollection :: Phantoms.TTerm Model.GeometryCollection -> Phantoms.TTerm Model.Geometry
geometryGeometryCollection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "geometryCollection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryLineString :: Phantoms.TTerm Model.LineString -> Phantoms.TTerm Model.Geometry
geometryLineString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lineString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryMultiLineString :: Phantoms.TTerm Model.MultiLineString -> Phantoms.TTerm Model.Geometry
geometryMultiLineString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiLineString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryMultiPoint :: Phantoms.TTerm Model.MultiPoint -> Phantoms.TTerm Model.Geometry
geometryMultiPoint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiPoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryMultiPolygon :: Phantoms.TTerm Model.MultiPolygon -> Phantoms.TTerm Model.Geometry
geometryMultiPolygon x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiPolygon"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryPoint :: Phantoms.TTerm Model.Point -> Phantoms.TTerm Model.Geometry
geometryPoint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "point"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryPolygon :: Phantoms.TTerm Model.Polygon -> Phantoms.TTerm Model.Geometry
geometryPolygon x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polygon"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

idNumber :: Phantoms.TTerm Double -> Phantoms.TTerm Model.Id
idNumber x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Id"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

idString :: Phantoms.TTerm String -> Phantoms.TTerm Model.Id
idString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Id"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lineString :: Phantoms.TTerm [Model.Position] -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.LineString
lineString coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.LineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

lineStringBbox :: Phantoms.TTerm Model.LineString -> Phantoms.TTerm (Maybe Model.BoundingBox)
lineStringBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.LineString"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lineStringCoordinates :: Phantoms.TTerm Model.LineString -> Phantoms.TTerm [Model.Position]
lineStringCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.LineString"),
        Core.projectionField = (Core.Name "coordinates")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lineStringWithBbox :: Phantoms.TTerm Model.LineString -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.LineString
lineStringWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.LineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.LineString"),
              Core.projectionField = (Core.Name "coordinates")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lineStringWithCoordinates :: Phantoms.TTerm Model.LineString -> Phantoms.TTerm [Model.Position] -> Phantoms.TTerm Model.LineString
lineStringWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.LineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.LineString"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiLineString :: Phantoms.TTerm [Model.LineString] -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.MultiLineString
multiLineString coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiLineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

multiLineStringBbox :: Phantoms.TTerm Model.MultiLineString -> Phantoms.TTerm (Maybe Model.BoundingBox)
multiLineStringBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiLineString"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiLineStringCoordinates :: Phantoms.TTerm Model.MultiLineString -> Phantoms.TTerm [Model.LineString]
multiLineStringCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiLineString"),
        Core.projectionField = (Core.Name "coordinates")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiLineStringWithBbox :: Phantoms.TTerm Model.MultiLineString -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.MultiLineString
multiLineStringWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiLineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiLineString"),
              Core.projectionField = (Core.Name "coordinates")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiLineStringWithCoordinates :: Phantoms.TTerm Model.MultiLineString -> Phantoms.TTerm [Model.LineString] -> Phantoms.TTerm Model.MultiLineString
multiLineStringWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiLineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiLineString"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiPoint :: Phantoms.TTerm [Model.Point] -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.MultiPoint
multiPoint coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPoint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

multiPointBbox :: Phantoms.TTerm Model.MultiPoint -> Phantoms.TTerm (Maybe Model.BoundingBox)
multiPointBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPoint"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPointCoordinates :: Phantoms.TTerm Model.MultiPoint -> Phantoms.TTerm [Model.Point]
multiPointCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPoint"),
        Core.projectionField = (Core.Name "coordinates")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPointWithBbox :: Phantoms.TTerm Model.MultiPoint -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.MultiPoint
multiPointWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPoint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPoint"),
              Core.projectionField = (Core.Name "coordinates")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiPointWithCoordinates :: Phantoms.TTerm Model.MultiPoint -> Phantoms.TTerm [Model.Point] -> Phantoms.TTerm Model.MultiPoint
multiPointWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPoint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPoint"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiPolygon :: Phantoms.TTerm [Model.Polygon] -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.MultiPolygon
multiPolygon coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPolygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

multiPolygonBbox :: Phantoms.TTerm Model.MultiPolygon -> Phantoms.TTerm (Maybe Model.BoundingBox)
multiPolygonBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPolygon"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPolygonCoordinates :: Phantoms.TTerm Model.MultiPolygon -> Phantoms.TTerm [Model.Polygon]
multiPolygonCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPolygon"),
        Core.projectionField = (Core.Name "coordinates")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPolygonWithBbox :: Phantoms.TTerm Model.MultiPolygon -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.MultiPolygon
multiPolygonWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPolygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPolygon"),
              Core.projectionField = (Core.Name "coordinates")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiPolygonWithCoordinates :: Phantoms.TTerm Model.MultiPolygon -> Phantoms.TTerm [Model.Polygon] -> Phantoms.TTerm Model.MultiPolygon
multiPolygonWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPolygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.MultiPolygon"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectFeature :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm Model.Object
objectFeature x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Object"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "feature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectFeatureCollection :: Phantoms.TTerm Model.FeatureCollection -> Phantoms.TTerm Model.Object
objectFeatureCollection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Object"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "featureCollection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectGeometry :: Phantoms.TTerm Model.Geometry -> Phantoms.TTerm Model.Object
objectGeometry x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Object"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "geometry"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

point :: Phantoms.TTerm Model.Position -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.Point
point coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Point"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

pointBbox :: Phantoms.TTerm Model.Point -> Phantoms.TTerm (Maybe Model.BoundingBox)
pointBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Point"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pointCoordinates :: Phantoms.TTerm Model.Point -> Phantoms.TTerm Model.Position
pointCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Point"),
        Core.projectionField = (Core.Name "coordinates")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pointWithBbox :: Phantoms.TTerm Model.Point -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.Point
pointWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Point"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Point"),
              Core.projectionField = (Core.Name "coordinates")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pointWithCoordinates :: Phantoms.TTerm Model.Point -> Phantoms.TTerm Model.Position -> Phantoms.TTerm Model.Point
pointWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Point"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Point"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

polygon :: Phantoms.TTerm [Model.Position] -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.Polygon
polygon coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Polygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

polygonBbox :: Phantoms.TTerm Model.Polygon -> Phantoms.TTerm (Maybe Model.BoundingBox)
polygonBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Polygon"),
        Core.projectionField = (Core.Name "bbox")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polygonCoordinates :: Phantoms.TTerm Model.Polygon -> Phantoms.TTerm [Model.Position]
polygonCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Polygon"),
        Core.projectionField = (Core.Name "coordinates")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polygonWithBbox :: Phantoms.TTerm Model.Polygon -> Phantoms.TTerm (Maybe Model.BoundingBox) -> Phantoms.TTerm Model.Polygon
polygonWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Polygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Polygon"),
              Core.projectionField = (Core.Name "coordinates")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

polygonWithCoordinates :: Phantoms.TTerm Model.Polygon -> Phantoms.TTerm [Model.Position] -> Phantoms.TTerm Model.Polygon
polygonWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Polygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Polygon"),
              Core.projectionField = (Core.Name "bbox")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

position :: Phantoms.TTerm Double -> Phantoms.TTerm Double -> Phantoms.TTerm (Maybe Double) -> Phantoms.TTerm Model.Position
position latitude longitude altitude =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "latitude"),
          Core.fieldTerm = (Phantoms.unTTerm latitude)},
        Core.Field {
          Core.fieldName = (Core.Name "longitude"),
          Core.fieldTerm = (Phantoms.unTTerm longitude)},
        Core.Field {
          Core.fieldName = (Core.Name "altitude"),
          Core.fieldTerm = (Phantoms.unTTerm altitude)}]}))

positionAltitude :: Phantoms.TTerm Model.Position -> Phantoms.TTerm (Maybe Double)
positionAltitude x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
        Core.projectionField = (Core.Name "altitude")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

positionLatitude :: Phantoms.TTerm Model.Position -> Phantoms.TTerm Double
positionLatitude x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
        Core.projectionField = (Core.Name "latitude")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

positionLongitude :: Phantoms.TTerm Model.Position -> Phantoms.TTerm Double
positionLongitude x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
        Core.projectionField = (Core.Name "longitude")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

positionWithAltitude :: Phantoms.TTerm Model.Position -> Phantoms.TTerm (Maybe Double) -> Phantoms.TTerm Model.Position
positionWithAltitude original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "latitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
              Core.projectionField = (Core.Name "latitude")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "longitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
              Core.projectionField = (Core.Name "longitude")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "altitude"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

positionWithLatitude :: Phantoms.TTerm Model.Position -> Phantoms.TTerm Double -> Phantoms.TTerm Model.Position
positionWithLatitude original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "latitude"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "longitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
              Core.projectionField = (Core.Name "longitude")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "altitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
              Core.projectionField = (Core.Name "altitude")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

positionWithLongitude :: Phantoms.TTerm Model.Position -> Phantoms.TTerm Double -> Phantoms.TTerm Model.Position
positionWithLongitude original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "latitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
              Core.projectionField = (Core.Name "latitude")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "longitude"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "altitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.geojson.model.Position"),
              Core.projectionField = (Core.Name "altitude")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unBoundingBox :: Phantoms.TTerm Model.BoundingBox -> Phantoms.TTerm [Model.CoordinateRange]
unBoundingBox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.geojson.model.BoundingBox")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
