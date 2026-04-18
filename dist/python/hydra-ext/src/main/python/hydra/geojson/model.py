# Note: this is an automatically generated file. Do not edit.

r"""A GeoJSON model based on the specification at https://www.rfc-editor.org/rfc/rfc7946. This model provides some additional structure beyond the JSON encoding described in the specification; For example, it provides MultiPoint not as an object with an array of coordinates which encode points, but as an array of Points. A BoundingBox is not simply an list of numbers, but a list of CoordinateRanges."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.json.model

class BoundingBox(Node["frozenlist[CoordinateRange]"]):
    r"""A GeoJSON object MAY have a member named "bbox" to include information on the coordinate range for its Geometries, Features, or FeatureCollections.  The value of the bbox member MUST be an array of length 2*n where n is the number of dimensions represented in the contained geometries, with all axes of the most southwesterly point followed by all axes of the more northeasterly point.  The axes order of a bbox follows the axes order of geometries."""

BoundingBox.TYPE_ = hydra.core.Name("hydra.geojson.model.BoundingBox")

@dataclass(frozen=True)
class CoordinateRange:
    min: Decimal
    max: Decimal

    TYPE_ = hydra.core.Name("hydra.geojson.model.CoordinateRange")
    MIN = hydra.core.Name("min")
    MAX = hydra.core.Name("max")

@dataclass(frozen=True)
class Feature:
    r"""A Feature object represents a spatially bounded thing.  Every Feature object is a GeoJSON object no matter where it occurs in a GeoJSON text."""

    geometry: Annotated[Maybe[Geometry], "A Feature object has a member with the name \"geometry\".  The value of the geometry member SHALL be either a Geometry object as defined above or, in the case that the Feature is unlocated, a JSON null value."]
    properties: Annotated[Maybe[FrozenDict[str, hydra.json.model.Value]], "A Feature object has a member with the name \"properties\".  The value of the properties member is an object (any JSON object or a JSON null value)."]
    id: Annotated[Maybe[Id], "If a Feature has a commonly used identifier, that identifier SHOULD be included as a member of the Feature object with the name \"id\", and the value of this member is either a JSON string or number."]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.Feature")
    GEOMETRY = hydra.core.Name("geometry")
    PROPERTIES = hydra.core.Name("properties")
    ID = hydra.core.Name("id")
    BBOX = hydra.core.Name("bbox")

@dataclass(frozen=True)
class FeatureCollection:
    r"""A GeoJSON object with the type "FeatureCollection" is a FeatureCollection object.  A FeatureCollection object has a member with the name "features".  The value of "features" is a JSON array. Each element of the array is a Feature object as defined above.  It is possible for this array to be empty."""

    features: frozenlist[Feature]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.FeatureCollection")
    FEATURES = hydra.core.Name("features")
    BBOX = hydra.core.Name("bbox")

@dataclass(frozen=True)
class GeometryCollection:
    r"""A GeoJSON object with type "GeometryCollection" is a Geometry object. A GeometryCollection has a member with the name "geometries".  The value of "geometries" is an array.  Each element of this array is a GeoJSON Geometry object.  It is possible for this array to be empty."""

    geometries: frozenlist[Geometry]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.GeometryCollection")
    GEOMETRIES = hydra.core.Name("geometries")
    BBOX = hydra.core.Name("bbox")

class GeometryPoint(Node["Point"]):
    ...

class GeometryMultiPoint(Node["MultiPoint"]):
    ...

class GeometryLineString(Node["LineString"]):
    ...

class GeometryMultiLineString(Node["MultiLineString"]):
    ...

class GeometryPolygon(Node["Polygon"]):
    ...

class GeometryMultiPolygon(Node["MultiPolygon"]):
    ...

class GeometryGeometryCollection(Node["GeometryCollection"]):
    ...

class _GeometryMeta(type):
    def __getitem__(cls, item):
        return object

# A Geometry object represents points, curves, and surfaces in coordinate space.  Every Geometry object is a GeoJSON object no matter where it occurs in a GeoJSON text.
class Geometry(metaclass=_GeometryMeta):
    r"""GeometryPoint | GeometryMultiPoint | GeometryLineString | GeometryMultiLineString | GeometryPolygon | GeometryMultiPolygon | GeometryGeometryCollection"""

    TYPE_ = hydra.core.Name("hydra.geojson.model.Geometry")
    POINT = hydra.core.Name("point")
    MULTI_POINT = hydra.core.Name("multiPoint")
    LINE_STRING = hydra.core.Name("lineString")
    MULTI_LINE_STRING = hydra.core.Name("multiLineString")
    POLYGON = hydra.core.Name("polygon")
    MULTI_POLYGON = hydra.core.Name("multiPolygon")
    GEOMETRY_COLLECTION = hydra.core.Name("geometryCollection")

class IdNumber(Node[Decimal]):
    ...

class IdString(Node[str]):
    ...

class _IdMeta(type):
    def __getitem__(cls, item):
        return object

class Id(metaclass=_IdMeta):
    r"""IdNumber | IdString"""

    TYPE_ = hydra.core.Name("hydra.geojson.model.Id")
    NUMBER = hydra.core.Name("number")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class LineString:
    r"""For type "LineString", the "coordinates" member is an array of two or more positions."""

    coordinates: frozenlist[Position]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.LineString")
    COORDINATES = hydra.core.Name("coordinates")
    BBOX = hydra.core.Name("bbox")

@dataclass(frozen=True)
class MultiLineString:
    r"""For type "MultiLineString", the "coordinates" member is an array of LineString coordinate arrays."""

    coordinates: frozenlist[LineString]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.MultiLineString")
    COORDINATES = hydra.core.Name("coordinates")
    BBOX = hydra.core.Name("bbox")

@dataclass(frozen=True)
class MultiPoint:
    r"""For type "MultiPoint", the "coordinates" member is an array of positions."""

    coordinates: frozenlist[Point]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.MultiPoint")
    COORDINATES = hydra.core.Name("coordinates")
    BBOX = hydra.core.Name("bbox")

@dataclass(frozen=True)
class MultiPolygon:
    r"""For type "MultiPolygon", the "coordinates" member is an array of Polygon coordinate arrays."""

    coordinates: frozenlist[Polygon]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.MultiPolygon")
    COORDINATES = hydra.core.Name("coordinates")
    BBOX = hydra.core.Name("bbox")

class ObjectGeometry(Node["Geometry"]):
    ...

class ObjectFeature(Node["Feature"]):
    ...

class ObjectFeatureCollection(Node["FeatureCollection"]):
    ...

class _ObjectMeta(type):
    def __getitem__(cls, item):
        return object

# A GeoJSON object represents a Geometry, Feature, or collection of Features.
class Object(metaclass=_ObjectMeta):
    r"""ObjectGeometry | ObjectFeature | ObjectFeatureCollection"""

    TYPE_ = hydra.core.Name("hydra.geojson.model.Object")
    GEOMETRY = hydra.core.Name("geometry")
    FEATURE = hydra.core.Name("feature")
    FEATURE_COLLECTION = hydra.core.Name("featureCollection")

@dataclass(frozen=True)
class Point:
    r"""For type "Point", the "coordinates" member is a single position."""

    coordinates: Position
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.Point")
    COORDINATES = hydra.core.Name("coordinates")
    BBOX = hydra.core.Name("bbox")

@dataclass(frozen=True)
class Polygon:
    r"""For type "Polygon", the "coordinates" member MUST be an array of linear ring coordinate arrays.
    For Polygons with more than one of these rings, the first MUST be the exterior ring, and any others MUST be interior rings.  The exterior ring bounds the surface, and the interior rings (if present) bound holes within the surface."""

    coordinates: frozenlist[Position]
    bbox: Maybe[BoundingBox]

    TYPE_ = hydra.core.Name("hydra.geojson.model.Polygon")
    COORDINATES = hydra.core.Name("coordinates")
    BBOX = hydra.core.Name("bbox")

@dataclass(frozen=True)
class Position:
    r"""A position is an array of numbers.  There MUST be two or more elements.  The first two elements are longitude and latitude, or easting and northing, precisely in that order and using decimal numbers.  Altitude or elevation MAY be included as an optional third element."""

    latitude: Decimal
    longitude: Decimal
    altitude: Maybe[Decimal]

    TYPE_ = hydra.core.Name("hydra.geojson.model.Position")
    LATITUDE = hydra.core.Name("latitude")
    LONGITUDE = hydra.core.Name("longitude")
    ALTITUDE = hydra.core.Name("altitude")
