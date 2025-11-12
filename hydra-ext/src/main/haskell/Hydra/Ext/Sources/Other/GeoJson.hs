module Hydra.Ext.Sources.Other.GeoJson where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types
import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Meta        as Meta
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


geoJsonModule :: Module
geoJsonModule = Module ns elements [Json.module_] [] $
    Just ("A GeoJSON model based on the specification at https://www.rfc-editor.org/rfc/rfc7946. " ++
          "This model provides some additional structure beyond the JSON encoding described in the specification; " ++
          "For example, it provides MultiPoint not as an object with an array of coordinates which encode points, " ++
          "but as an array of Points. A BoundingBox is not simply an list of numbers, but a list of CoordinateRanges.")
  where
    ns = Namespace "hydra.ext.org.geojson.model"
    def = datatype ns
    geoj = typeref ns
    json = typeref $ moduleNamespace Json.module_

    elements = [

      def "BoundingBox" $
        doc ("A GeoJSON object MAY have a member named \"bbox\" to include " ++
             "information on the coordinate range for its Geometries, Features, or " ++
             "FeatureCollections.  The value of the bbox member MUST be an array of " ++
             "length 2*n where n is the number of dimensions represented in the " ++
             "contained geometries, with all axes of the most southwesterly point " ++
             "followed by all axes of the more northeasterly point.  The axes order " ++
             "of a bbox follows the axes order of geometries.") $
        wrap $ list $ geoj "CoordinateRange",

      def "CoordinateRange" $
        record [
          "min">: bigfloat,
          "max">: bigfloat],

      def "Feature" $
       doc ("A Feature object represents a spatially bounded thing.  Every Feature " ++
            "object is a GeoJSON object no matter where it occurs in a GeoJSON " ++
            "text.") $
       record [
        "geometry">:
          doc ("A Feature object has a member with the name \"geometry\".  The value " ++
               "of the geometry member SHALL be either a Geometry object as " ++
               "defined above or, in the case that the Feature is unlocated, a " ++
               "JSON null value.") $
          optional $ geoj "Geometry",
        "properties">:
          doc ("A Feature object has a member with the name \"properties\".  The " ++
               "value of the properties member is an object (any JSON object or a " ++
               "JSON null value).") $
          optional $ Types.map string (json "Value"),
        "id">:
          doc ("If a Feature has a commonly used identifier, that identifier " ++
               "SHOULD be included as a member of the Feature object with the name " ++
               "\"id\", and the value of this member is either a JSON string or " ++
               "number.") $
          optional $ geoj "Id",
        "bbox">: optional $ geoj "BoundingBox"],

      def "FeatureCollection" $
        doc ("A GeoJSON object with the type \"FeatureCollection\" is a " ++
             "FeatureCollection object.  A FeatureCollection object has a member " ++
             "with the name \"features\".  The value of \"features\" is a JSON array. " ++
             "Each element of the array is a Feature object as defined above.  It " ++
             "is possible for this array to be empty.") $
        record [
          "features">: list $ geoj "Feature",
          "bbox">: optional $ geoj "BoundingBox"],

      def "GeometryCollection" $
        doc ("A GeoJSON object with type \"GeometryCollection\" is a Geometry object. " ++
             "A GeometryCollection has a member with the name \"geometries\".  The " ++
             "value of \"geometries\" is an array.  Each element of this array is a " ++
             "GeoJSON Geometry object.  It is possible for this array to be empty.") $
        record [
          "geometries">: list $ geoj "Geometry",
          "bbox">: optional $ geoj "BoundingBox"],

      def "Geometry" $
        doc ("A Geometry object represents points, curves, and surfaces in " ++
             "coordinate space.  Every Geometry object is a GeoJSON object no " ++
             "matter where it occurs in a GeoJSON text.") $
        union [
          "point">: geoj "Point",
          "multiPoint">: geoj "MultiPoint",
          "lineString">: geoj "LineString",
          "multiLineString">: geoj "MultiLineString",
          "polygon">: geoj "Polygon",
          "multiPolygon">: geoj "MultiPolygon",
          "geometryCollection">: geoj "GeometryCollection"],

      def "Id" $
        union [
          "number">: bigfloat,
          "string">: string],

      def "LineString" $
        doc ("For type \"LineString\", the \"coordinates\" member is an array of two or " ++
             "more positions.") $
        record [
          "coordinates">: list $ geoj "Position",
          "bbox">: optional $ geoj "BoundingBox"],

      def "MultiLineString" $
        doc ("For type \"MultiLineString\", the \"coordinates\" member is an array of " ++
             "LineString coordinate arrays.") $
        record [
          "coordinates">: list $ geoj "LineString",
          "bbox">: optional $ geoj "BoundingBox"],

      def "MultiPoint" $
        doc ("For type \"MultiPoint\", the \"coordinates\" member is an array of " ++
             "positions.") $
        record [
          "coordinates">: list $ geoj "Point",
          "bbox">: optional $ geoj "BoundingBox"],

      def "MultiPolygon" $
        doc ("For type \"MultiPolygon\", the \"coordinates\" member is an array of " ++
             "Polygon coordinate arrays.") $
        record [
          "coordinates">: list $ geoj "Polygon",
          "bbox">: optional $ geoj "BoundingBox"],

      def "Object" $
        doc ("A GeoJSON object represents a Geometry, Feature, or collection of " ++
             "Features.") $
        union [
          "geometry">: geoj "Geometry",
          "feature">: geoj "Feature",
          "featureCollection">: geoj "FeatureCollection"],

      def "Point" $
        doc ("For type \"Point\", the \"coordinates\" member is a single position.") $
        record [
          "coordinates">: geoj "Position",
          "bbox">: optional $ geoj "BoundingBox"],

      def "Polygon" $
        doc ("For type \"Polygon\", the \"coordinates\" member MUST be an array of " ++
             "linear ring coordinate arrays.\n" ++
             "For Polygons with more than one of these rings, the first MUST be " ++
             "the exterior ring, and any others MUST be interior rings.  The " ++
             "exterior ring bounds the surface, and the interior rings (if " ++
             "present) bound holes within the surface.") $
        record [
          "coordinates">: list $ geoj "Position",
          "bbox">: optional $ geoj "BoundingBox"],

      def "Position" $
        doc ("A position is an array of numbers.  There MUST be two or more " ++
             "elements.  The first two elements are longitude and latitude, or " ++
             "easting and northing, precisely in that order and using decimal " ++
             "numbers.  Altitude or elevation MAY be included as an optional third " ++
             "element.") $
        record [
          "latitude">: bigfloat,
          "longitude">: bigfloat,
          "altitude">: optional bigfloat]]
