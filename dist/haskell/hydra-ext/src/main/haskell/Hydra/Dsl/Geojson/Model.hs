-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.geojson.model

module Hydra.Dsl.Geojson.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Geojson.Model as GeojsonModel
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

boundingBox :: Phantoms.TTerm [GeojsonModel.CoordinateRange] -> Phantoms.TTerm GeojsonModel.BoundingBox
boundingBox x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.geojson.model.BoundingBox"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

coordinateRange :: Phantoms.TTerm Double -> Phantoms.TTerm Double -> Phantoms.TTerm GeojsonModel.CoordinateRange
coordinateRange min max =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.CoordinateRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm min)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm max)}]}))

coordinateRangeMax :: Phantoms.TTerm GeojsonModel.CoordinateRange -> Phantoms.TTerm Double
coordinateRangeMax x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.CoordinateRange"),
        Core.projectionField = (Core.Name "max")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coordinateRangeMin :: Phantoms.TTerm GeojsonModel.CoordinateRange -> Phantoms.TTerm Double
coordinateRangeMin x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.CoordinateRange"),
        Core.projectionField = (Core.Name "min")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coordinateRangeWithMax :: Phantoms.TTerm GeojsonModel.CoordinateRange -> Phantoms.TTerm Double -> Phantoms.TTerm GeojsonModel.CoordinateRange
coordinateRangeWithMax original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.CoordinateRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.CoordinateRange"),
              Core.projectionField = (Core.Name "min")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

coordinateRangeWithMin :: Phantoms.TTerm GeojsonModel.CoordinateRange -> Phantoms.TTerm Double -> Phantoms.TTerm GeojsonModel.CoordinateRange
coordinateRangeWithMin original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.CoordinateRange"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "min"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.CoordinateRange"),
              Core.projectionField = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

feature :: Phantoms.TTerm (Maybe GeojsonModel.Geometry) -> Phantoms.TTerm (Maybe (M.Map String JsonModel.Value)) -> Phantoms.TTerm (Maybe GeojsonModel.Id) -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.Feature
feature geometry properties id bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Feature"),
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

featureBbox :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
featureBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureCollection :: Phantoms.TTerm [GeojsonModel.Feature] -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.FeatureCollection
featureCollection features bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.FeatureCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "features"),
          Core.fieldTerm = (Phantoms.unTTerm features)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

featureCollectionBbox :: Phantoms.TTerm GeojsonModel.FeatureCollection -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
featureCollectionBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.FeatureCollection"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureCollectionFeatures :: Phantoms.TTerm GeojsonModel.FeatureCollection -> Phantoms.TTerm [GeojsonModel.Feature]
featureCollectionFeatures x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.FeatureCollection"),
        Core.projectionField = (Core.Name "features")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureCollectionWithBbox :: Phantoms.TTerm GeojsonModel.FeatureCollection -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.FeatureCollection
featureCollectionWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.FeatureCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "features"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.FeatureCollection"),
              Core.projectionField = (Core.Name "features")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

featureCollectionWithFeatures :: Phantoms.TTerm GeojsonModel.FeatureCollection -> Phantoms.TTerm [GeojsonModel.Feature] -> Phantoms.TTerm GeojsonModel.FeatureCollection
featureCollectionWithFeatures original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.FeatureCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "features"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.FeatureCollection"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featureGeometry :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe GeojsonModel.Geometry)
featureGeometry x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
        Core.projectionField = (Core.Name "geometry")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureId :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe GeojsonModel.Id)
featureId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureProperties :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe (M.Map String JsonModel.Value))
featureProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

featureWithBbox :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.Feature
featureWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "geometry")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

featureWithGeometry :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe GeojsonModel.Geometry) -> Phantoms.TTerm GeojsonModel.Feature
featureWithGeometry original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featureWithId :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe GeojsonModel.Id) -> Phantoms.TTerm GeojsonModel.Feature
featureWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "geometry")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

featureWithProperties :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm (Maybe (M.Map String JsonModel.Value)) -> Phantoms.TTerm GeojsonModel.Feature
featureWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Feature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometry"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "geometry")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Feature"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

geometryCollection :: Phantoms.TTerm [GeojsonModel.Geometry] -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.GeometryCollection
geometryCollection geometries bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.GeometryCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometries"),
          Core.fieldTerm = (Phantoms.unTTerm geometries)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

geometryCollectionBbox :: Phantoms.TTerm GeojsonModel.GeometryCollection -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
geometryCollectionBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.GeometryCollection"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

geometryCollectionGeometries :: Phantoms.TTerm GeojsonModel.GeometryCollection -> Phantoms.TTerm [GeojsonModel.Geometry]
geometryCollectionGeometries x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.GeometryCollection"),
        Core.projectionField = (Core.Name "geometries")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

geometryCollectionWithBbox :: Phantoms.TTerm GeojsonModel.GeometryCollection -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.GeometryCollection
geometryCollectionWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.GeometryCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometries"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.GeometryCollection"),
              Core.projectionField = (Core.Name "geometries")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

geometryCollectionWithGeometries :: Phantoms.TTerm GeojsonModel.GeometryCollection -> Phantoms.TTerm [GeojsonModel.Geometry] -> Phantoms.TTerm GeojsonModel.GeometryCollection
geometryCollectionWithGeometries original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.GeometryCollection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "geometries"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.GeometryCollection"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

geometryGeometryCollection :: Phantoms.TTerm GeojsonModel.GeometryCollection -> Phantoms.TTerm GeojsonModel.Geometry
geometryGeometryCollection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "geometryCollection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryLineString :: Phantoms.TTerm GeojsonModel.LineString -> Phantoms.TTerm GeojsonModel.Geometry
geometryLineString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lineString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryMultiLineString :: Phantoms.TTerm GeojsonModel.MultiLineString -> Phantoms.TTerm GeojsonModel.Geometry
geometryMultiLineString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiLineString"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryMultiPoint :: Phantoms.TTerm GeojsonModel.MultiPoint -> Phantoms.TTerm GeojsonModel.Geometry
geometryMultiPoint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiPoint"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryMultiPolygon :: Phantoms.TTerm GeojsonModel.MultiPolygon -> Phantoms.TTerm GeojsonModel.Geometry
geometryMultiPolygon x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiPolygon"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryPoint :: Phantoms.TTerm GeojsonModel.Point -> Phantoms.TTerm GeojsonModel.Geometry
geometryPoint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "point"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

geometryPolygon :: Phantoms.TTerm GeojsonModel.Polygon -> Phantoms.TTerm GeojsonModel.Geometry
geometryPolygon x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Geometry"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "polygon"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

idNumber :: Phantoms.TTerm Double -> Phantoms.TTerm GeojsonModel.Id
idNumber x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Id"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

idString :: Phantoms.TTerm String -> Phantoms.TTerm GeojsonModel.Id
idString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Id"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lineString :: Phantoms.TTerm [GeojsonModel.Position] -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.LineString
lineString coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.LineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

lineStringBbox :: Phantoms.TTerm GeojsonModel.LineString -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
lineStringBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.LineString"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lineStringCoordinates :: Phantoms.TTerm GeojsonModel.LineString -> Phantoms.TTerm [GeojsonModel.Position]
lineStringCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.LineString"),
        Core.projectionField = (Core.Name "coordinates")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lineStringWithBbox :: Phantoms.TTerm GeojsonModel.LineString -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.LineString
lineStringWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.LineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.LineString"),
              Core.projectionField = (Core.Name "coordinates")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lineStringWithCoordinates :: Phantoms.TTerm GeojsonModel.LineString -> Phantoms.TTerm [GeojsonModel.Position] -> Phantoms.TTerm GeojsonModel.LineString
lineStringWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.LineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.LineString"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiLineString :: Phantoms.TTerm [GeojsonModel.LineString] -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.MultiLineString
multiLineString coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiLineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

multiLineStringBbox :: Phantoms.TTerm GeojsonModel.MultiLineString -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
multiLineStringBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiLineString"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiLineStringCoordinates :: Phantoms.TTerm GeojsonModel.MultiLineString -> Phantoms.TTerm [GeojsonModel.LineString]
multiLineStringCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiLineString"),
        Core.projectionField = (Core.Name "coordinates")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiLineStringWithBbox :: Phantoms.TTerm GeojsonModel.MultiLineString -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.MultiLineString
multiLineStringWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiLineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiLineString"),
              Core.projectionField = (Core.Name "coordinates")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiLineStringWithCoordinates :: Phantoms.TTerm GeojsonModel.MultiLineString -> Phantoms.TTerm [GeojsonModel.LineString] -> Phantoms.TTerm GeojsonModel.MultiLineString
multiLineStringWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiLineString"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiLineString"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiPoint :: Phantoms.TTerm [GeojsonModel.Point] -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.MultiPoint
multiPoint coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiPoint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

multiPointBbox :: Phantoms.TTerm GeojsonModel.MultiPoint -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
multiPointBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPoint"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPointCoordinates :: Phantoms.TTerm GeojsonModel.MultiPoint -> Phantoms.TTerm [GeojsonModel.Point]
multiPointCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPoint"),
        Core.projectionField = (Core.Name "coordinates")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPointWithBbox :: Phantoms.TTerm GeojsonModel.MultiPoint -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.MultiPoint
multiPointWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiPoint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPoint"),
              Core.projectionField = (Core.Name "coordinates")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiPointWithCoordinates :: Phantoms.TTerm GeojsonModel.MultiPoint -> Phantoms.TTerm [GeojsonModel.Point] -> Phantoms.TTerm GeojsonModel.MultiPoint
multiPointWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiPoint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPoint"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiPolygon :: Phantoms.TTerm [GeojsonModel.Polygon] -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.MultiPolygon
multiPolygon coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiPolygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

multiPolygonBbox :: Phantoms.TTerm GeojsonModel.MultiPolygon -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
multiPolygonBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPolygon"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPolygonCoordinates :: Phantoms.TTerm GeojsonModel.MultiPolygon -> Phantoms.TTerm [GeojsonModel.Polygon]
multiPolygonCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPolygon"),
        Core.projectionField = (Core.Name "coordinates")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPolygonWithBbox :: Phantoms.TTerm GeojsonModel.MultiPolygon -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.MultiPolygon
multiPolygonWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiPolygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPolygon"),
              Core.projectionField = (Core.Name "coordinates")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiPolygonWithCoordinates :: Phantoms.TTerm GeojsonModel.MultiPolygon -> Phantoms.TTerm [GeojsonModel.Polygon] -> Phantoms.TTerm GeojsonModel.MultiPolygon
multiPolygonWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.MultiPolygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.MultiPolygon"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

objectFeature :: Phantoms.TTerm GeojsonModel.Feature -> Phantoms.TTerm GeojsonModel.Object
objectFeature x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Object"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "feature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectFeatureCollection :: Phantoms.TTerm GeojsonModel.FeatureCollection -> Phantoms.TTerm GeojsonModel.Object
objectFeatureCollection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Object"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "featureCollection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectGeometry :: Phantoms.TTerm GeojsonModel.Geometry -> Phantoms.TTerm GeojsonModel.Object
objectGeometry x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.geojson.model.Object"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "geometry"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

point :: Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.Point
point coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Point"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

pointBbox :: Phantoms.TTerm GeojsonModel.Point -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
pointBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Point"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pointCoordinates :: Phantoms.TTerm GeojsonModel.Point -> Phantoms.TTerm GeojsonModel.Position
pointCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Point"),
        Core.projectionField = (Core.Name "coordinates")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pointWithBbox :: Phantoms.TTerm GeojsonModel.Point -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.Point
pointWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Point"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Point"),
              Core.projectionField = (Core.Name "coordinates")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pointWithCoordinates :: Phantoms.TTerm GeojsonModel.Point -> Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm GeojsonModel.Point
pointWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Point"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Point"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

polygon :: Phantoms.TTerm [GeojsonModel.Position] -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.Polygon
polygon coordinates bbox =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Polygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm coordinates)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm bbox)}]}))

polygonBbox :: Phantoms.TTerm GeojsonModel.Polygon -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox)
polygonBbox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Polygon"),
        Core.projectionField = (Core.Name "bbox")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polygonCoordinates :: Phantoms.TTerm GeojsonModel.Polygon -> Phantoms.TTerm [GeojsonModel.Position]
polygonCoordinates x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Polygon"),
        Core.projectionField = (Core.Name "coordinates")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

polygonWithBbox :: Phantoms.TTerm GeojsonModel.Polygon -> Phantoms.TTerm (Maybe GeojsonModel.BoundingBox) -> Phantoms.TTerm GeojsonModel.Polygon
polygonWithBbox original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Polygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Polygon"),
              Core.projectionField = (Core.Name "coordinates")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

polygonWithCoordinates :: Phantoms.TTerm GeojsonModel.Polygon -> Phantoms.TTerm [GeojsonModel.Position] -> Phantoms.TTerm GeojsonModel.Polygon
polygonWithCoordinates original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Polygon"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "coordinates"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bbox"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Polygon"),
              Core.projectionField = (Core.Name "bbox")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

position :: Phantoms.TTerm Double -> Phantoms.TTerm Double -> Phantoms.TTerm (Maybe Double) -> Phantoms.TTerm GeojsonModel.Position
position latitude longitude altitude =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Position"),
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

positionAltitude :: Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm (Maybe Double)
positionAltitude x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
        Core.projectionField = (Core.Name "altitude")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

positionLatitude :: Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm Double
positionLatitude x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
        Core.projectionField = (Core.Name "latitude")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

positionLongitude :: Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm Double
positionLongitude x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
        Core.projectionField = (Core.Name "longitude")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

positionWithAltitude :: Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm (Maybe Double) -> Phantoms.TTerm GeojsonModel.Position
positionWithAltitude original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Position"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "latitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
              Core.projectionField = (Core.Name "latitude")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "longitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
              Core.projectionField = (Core.Name "longitude")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "altitude"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

positionWithLatitude :: Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm Double -> Phantoms.TTerm GeojsonModel.Position
positionWithLatitude original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Position"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "latitude"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "longitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
              Core.projectionField = (Core.Name "longitude")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "altitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
              Core.projectionField = (Core.Name "altitude")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

positionWithLongitude :: Phantoms.TTerm GeojsonModel.Position -> Phantoms.TTerm Double -> Phantoms.TTerm GeojsonModel.Position
positionWithLongitude original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.geojson.model.Position"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "latitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
              Core.projectionField = (Core.Name "latitude")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "longitude"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "altitude"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.geojson.model.Position"),
              Core.projectionField = (Core.Name "altitude")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unBoundingBox :: Phantoms.TTerm GeojsonModel.BoundingBox -> Phantoms.TTerm [GeojsonModel.CoordinateRange]
unBoundingBox x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.geojson.model.BoundingBox")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
