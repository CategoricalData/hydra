module Hydra.Ext.Sources.Other.GeoJson where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import qualified Hydra.Sources.Json.Model        as JsonModel


ns :: Namespace
ns = Namespace "hydra.ext.org.geojson.model"

define :: String -> Type -> Binding
define = defineType ns

geoj :: String -> Type
geoj = typeref ns

json :: String -> Type
json = typeref $ JsonModel.ns

module_ :: Module
module_ = Module ns elements [JsonModel.ns] [] $
    Just ("A GeoJSON model based on the specification at https://www.rfc-editor.org/rfc/rfc7946. " ++
          "This model provides some additional structure beyond the JSON encoding described in the specification; " ++
          "For example, it provides MultiPoint not as an object with an array of coordinates which encode points, " ++
          "but as an array of Points. A BoundingBox is not simply an list of numbers, but a list of CoordinateRanges.")
  where
    elements = [
      boundingBox,
      coordinateRange,
      feature,
      featureCollection,
      geometryCollection,
      geometry,
      id_,
      lineString,
      multiLineString,
      multiPoint,
      multiPolygon,
      object_,
      point,
      polygon,
      position]

boundingBox :: Binding
boundingBox = define "BoundingBox" $
  doc ("A GeoJSON object MAY have a member named \"bbox\" to include " ++
       "information on the coordinate range for its Geometries, Features, or " ++
       "FeatureCollections.  The value of the bbox member MUST be an array of " ++
       "length 2*n where n is the number of dimensions represented in the " ++
       "contained geometries, with all axes of the most southwesterly point " ++
       "followed by all axes of the more northeasterly point.  The axes order " ++
       "of a bbox follows the axes order of geometries.") $
  T.wrap $ T.list $ geoj "CoordinateRange"

coordinateRange :: Binding
coordinateRange = define "CoordinateRange" $
  T.record [
    "min">: T.bigfloat,
    "max">: T.bigfloat]

feature :: Binding
feature = define "Feature" $
 doc ("A Feature object represents a spatially bounded thing.  Every Feature " ++
      "object is a GeoJSON object no matter where it occurs in a GeoJSON " ++
      "text.") $
 T.record [
  "geometry">:
    doc ("A Feature object has a member with the name \"geometry\".  The value " ++
         "of the geometry member SHALL be either a Geometry object as " ++
         "defined above or, in the case that the Feature is unlocated, a " ++
         "JSON null value.") $
    T.maybe $ geoj "Geometry",
  "properties">:
    doc ("A Feature object has a member with the name \"properties\".  The " ++
         "value of the properties member is an object (any JSON object or a " ++
         "JSON null value).") $
    T.maybe $ T.map T.string (json "Value"),
  "id">:
    doc ("If a Feature has a commonly used identifier, that identifier " ++
         "SHOULD be included as a member of the Feature object with the name " ++
         "\"id\", and the value of this member is either a JSON string or " ++
         "number.") $
    T.maybe $ geoj "Id",
  "bbox">: T.maybe $ geoj "BoundingBox"]

featureCollection :: Binding
featureCollection = define "FeatureCollection" $
  doc ("A GeoJSON object with the type \"FeatureCollection\" is a " ++
       "FeatureCollection object.  A FeatureCollection object has a member " ++
       "with the name \"features\".  The value of \"features\" is a JSON array. " ++
       "Each element of the array is a Feature object as defined above.  It " ++
       "is possible for this array to be empty.") $
  T.record [
    "features">: T.list $ geoj "Feature",
    "bbox">: T.maybe $ geoj "BoundingBox"]

geometryCollection :: Binding
geometryCollection = define "GeometryCollection" $
  doc ("A GeoJSON object with type \"GeometryCollection\" is a Geometry object. " ++
       "A GeometryCollection has a member with the name \"geometries\".  The " ++
       "value of \"geometries\" is an array.  Each element of this array is a " ++
       "GeoJSON Geometry object.  It is possible for this array to be empty.") $
  T.record [
    "geometries">: T.list $ geoj "Geometry",
    "bbox">: T.maybe $ geoj "BoundingBox"]

geometry :: Binding
geometry = define "Geometry" $
  doc ("A Geometry object represents points, curves, and surfaces in " ++
       "coordinate space.  Every Geometry object is a GeoJSON object no " ++
       "matter where it occurs in a GeoJSON text.") $
  T.union [
    "point">: geoj "Point",
    "multiPoint">: geoj "MultiPoint",
    "lineString">: geoj "LineString",
    "multiLineString">: geoj "MultiLineString",
    "polygon">: geoj "Polygon",
    "multiPolygon">: geoj "MultiPolygon",
    "geometryCollection">: geoj "GeometryCollection"]

id_ :: Binding
id_ = define "Id" $
  T.union [
    "number">: T.bigfloat,
    "string">: T.string]

lineString :: Binding
lineString = define "LineString" $
  doc ("For type \"LineString\", the \"coordinates\" member is an array of two or " ++
       "more positions.") $
  T.record [
    "coordinates">: T.list $ geoj "Position",
    "bbox">: T.maybe $ geoj "BoundingBox"]

multiLineString :: Binding
multiLineString = define "MultiLineString" $
  doc ("For type \"MultiLineString\", the \"coordinates\" member is an array of " ++
       "LineString coordinate arrays.") $
  T.record [
    "coordinates">: T.list $ geoj "LineString",
    "bbox">: T.maybe $ geoj "BoundingBox"]

multiPoint :: Binding
multiPoint = define "MultiPoint" $
  doc ("For type \"MultiPoint\", the \"coordinates\" member is an array of " ++
       "positions.") $
  T.record [
    "coordinates">: T.list $ geoj "Point",
    "bbox">: T.maybe $ geoj "BoundingBox"]

multiPolygon :: Binding
multiPolygon = define "MultiPolygon" $
  doc ("For type \"MultiPolygon\", the \"coordinates\" member is an array of " ++
       "Polygon coordinate arrays.") $
  T.record [
    "coordinates">: T.list $ geoj "Polygon",
    "bbox">: T.maybe $ geoj "BoundingBox"]

object_ :: Binding
object_ = define "Object" $
  doc ("A GeoJSON object represents a Geometry, Feature, or collection of " ++
       "Features.") $
  T.union [
    "geometry">: geoj "Geometry",
    "feature">: geoj "Feature",
    "featureCollection">: geoj "FeatureCollection"]

point :: Binding
point = define "Point" $
  doc ("For type \"Point\", the \"coordinates\" member is a single position.") $
  T.record [
    "coordinates">: geoj "Position",
    "bbox">: T.maybe $ geoj "BoundingBox"]

polygon :: Binding
polygon = define "Polygon" $
  doc ("For type \"Polygon\", the \"coordinates\" member MUST be an array of " ++
       "linear ring coordinate arrays.\n" ++
       "For Polygons with more than one of these rings, the first MUST be " ++
       "the exterior ring, and any others MUST be interior rings.  The " ++
       "exterior ring bounds the surface, and the interior rings (if " ++
       "present) bound holes within the surface.") $
  T.record [
    "coordinates">: T.list $ geoj "Position",
    "bbox">: T.maybe $ geoj "BoundingBox"]

position :: Binding
position = define "Position" $
  doc ("A position is an array of numbers.  There MUST be two or more " ++
       "elements.  The first two elements are longitude and latitude, or " ++
       "easting and northing, precisely in that order and using decimal " ++
       "numbers.  Altitude or elevation MAY be included as an optional third " ++
       "element.") $
  T.record [
    "latitude">: T.bigfloat,
    "longitude">: T.bigfloat,
    "altitude">: T.maybe T.bigfloat]
