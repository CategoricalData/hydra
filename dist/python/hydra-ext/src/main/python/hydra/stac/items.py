# Note: this is an automatically generated file. Do not edit.

r"""A model for SpatioTemporal Asset Catalog (STAC) Items. See https://github.com/radiantearth/stac-spec/blob/master/item-spec/item-spec.md."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.geojson.model
import hydra.iana.linkrelations

@dataclass(frozen=True)
class Asset:
    r"""An Asset is an object that contains a URI to data associated with the Item that can be downloaded or streamed. It is allowed to add additional fields."""

    href: Annotated[Uri, "URI to the asset object. Relative and absolute URI are both allowed."]
    title: Annotated[Maybe[str], "The displayed title for clients and users."]
    description: Annotated[Maybe[str], "A description of the Asset providing additional details, such as how it was processed or created. CommonMark 0.29 syntax MAY be used for rich text representation."]
    type: Annotated[Maybe[MediaType], "Media type of the asset. See the common media types in the best practice doc for commonly used asset types."]
    roles: Annotated[frozenlist[Role], "The semantic roles of the asset, similar to the use of rel in links."]

    TYPE_ = hydra.core.Name("hydra.stac.items.Asset")
    HREF = hydra.core.Name("href")
    TITLE = hydra.core.Name("title")
    DESCRIPTION = hydra.core.Name("description")
    TYPE = hydra.core.Name("type")
    ROLES = hydra.core.Name("roles")

@dataclass(frozen=True)
class Item:
    r"""This object describes a STAC Item. The fields id, type, bbox, geometry and properties are inherited from GeoJSON."""

    feature: hydra.geojson.model.Feature
    stac_version: Annotated[StacVersion, "The STAC version the Item implements"]
    stac_extensions: Annotated[frozenlist[Url], "A list of extensions the Item implements"]
    links: Annotated[frozenlist[Link], "List of link objects to resources and related URLs. A link with the rel set to self is strongly recommended."]
    assets: Annotated[FrozenDict[str, Asset], "Dictionary of asset objects that can be downloaded, each with a unique key."]
    collection: Annotated[Maybe[hydra.geojson.model.Id], "The id of the STAC Collection this Item references to (see collection relation type). This field is required if such a relation type is present and is not allowed otherwise. This field provides an easy way for a user to search for any Items that belong in a specified Collection. Must be a non-empty string."]

    TYPE_ = hydra.core.Name("hydra.stac.items.Item")
    FEATURE = hydra.core.Name("feature")
    STAC_VERSION = hydra.core.Name("stacVersion")
    STAC_EXTENSIONS = hydra.core.Name("stacExtensions")
    LINKS = hydra.core.Name("links")
    ASSETS = hydra.core.Name("assets")
    COLLECTION = hydra.core.Name("collection")

@dataclass(frozen=True)
class Link:
    r"""This object describes a relationship with another entity. Data providers are advised to be liberal with the links section, to describe things like the Catalog an Item is in, related Items, parent or child Items (modeled in different ways, like an 'acquisition' or derived data). It is allowed to add additional fields such as a title and type."""

    href: Annotated[Url, "The actual link in the format of an URL. Relative and absolute links are both allowed."]
    rel: Annotated[RelationType, "Relationship between the current document and the linked document. See chapter \"Relation types\" for more information."]
    type: Annotated[Maybe[MediaType], "Media type of the referenced entity."]
    title: Annotated[Maybe[str], "A human readable title to be used in rendered displays of the link."]

    TYPE_ = hydra.core.Name("hydra.stac.items.Link")
    HREF = hydra.core.Name("href")
    REL = hydra.core.Name("rel")
    TYPE = hydra.core.Name("type")
    TITLE = hydra.core.Name("title")

class MediaType(Node[str]):
    ...

MediaType.TYPE_ = hydra.core.Name("hydra.stac.items.MediaType")

class RelationTypeIana(Node["hydra.iana.linkrelations.LinkRelationType"]):
    ...

class RelationTypeStac(Node["StacRelationType"]):
    ...

class RelationTypeOther(Node[str]):
    ...

class _RelationTypeMeta(type):
    def __getitem__(cls, item):
        return object

# STAC Items use a variety of rel types in the link object, to describe the exact nature of the link between this Item and the entity it is linking to. It is recommended to use the official IANA Link Relation Types where possible. The following table explains places where STAC use custom rel types are used with Items. This happens where there is not a clear official option, or where STAC uses an official type but adds additional meaning for the STAC context.
class RelationType(metaclass=_RelationTypeMeta):
    r"""RelationTypeIana | RelationTypeStac | RelationTypeOther"""

    TYPE_ = hydra.core.Name("hydra.stac.items.RelationType")
    IANA = hydra.core.Name("iana")
    STAC = hydra.core.Name("stac")
    OTHER = hydra.core.Name("other")

class RoleThumbnail:
    r"""An asset that represents a thumbnail of the Item, typically a true color image (for Items with assets in the visible wavelengths), lower-resolution (typically smaller 600x600 pixels), and typically a JPEG or PNG (suitable for display in a web browser). Multiple assets may have this purpose, but it recommended that the type and roles be unique tuples. For example, Sentinel-2 L2A provides thumbnail images in both JPEG and JPEG2000 formats, and would be distinguished by their media types."""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RoleThumbnail)
    def __hash__(self):
        return hash("RoleThumbnail")

class RoleOverview:
    r"""An asset that represents a possibly larger view than the thumbnail of the Item, for example, a true color composite of multi-band data."""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RoleOverview)
    def __hash__(self):
        return hash("RoleOverview")

class RoleData:
    r"""The data itself. This is a suggestion for a common role for data files to be used in case data providers don't come up with their own names and semantics."""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RoleData)
    def __hash__(self):
        return hash("RoleData")

class RoleMetadata:
    r"""A metadata sidecar file describing the data in this Item, for example the Landsat-8 MTL file."""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RoleMetadata)
    def __hash__(self):
        return hash("RoleMetadata")

class RoleOther(Node[str]):
    ...

class _RoleMeta(type):
    def __getitem__(cls, item):
        return object

# The roles field is used to describe the purpose of each asset. It is recommended to include one for every asset, to give users a sense of why they might want to make use of the asset. There are some emerging standards that enable clients to take particular action when they encounter particular roles, listed below. But implementors are encouraged to come up with their own terms to describe the role.
class Role(metaclass=_RoleMeta):
    r"""RoleThumbnail | RoleOverview | RoleData | RoleMetadata | RoleOther"""

    TYPE_ = hydra.core.Name("hydra.stac.items.Role")
    THUMBNAIL = hydra.core.Name("thumbnail")
    OVERVIEW = hydra.core.Name("overview")
    DATA = hydra.core.Name("data")
    METADATA = hydra.core.Name("metadata")
    OTHER = hydra.core.Name("other")

class StacRelationType(Enum):
    SELF = hydra.core.Name("self")
    r"""STRONGLY RECOMMENDED. Absolute URL to the Item if it is available at a public URL. This is particularly useful when in a download package that includes metadata, so that the downstream user can know where the data has come from."""

    ROOT = hydra.core.Name("root")
    r"""URL to the root STAC entity (Catalog or Collection)."""

    PARENT = hydra.core.Name("parent")
    r"""URL to the parent STAC entity (Catalog or Collection)."""

    COLLECTION = hydra.core.Name("collection")
    r"""STRONGLY RECOMMENDED. URL to a Collection. Absolute URLs should be used whenever possible. The referenced Collection is STRONGLY RECOMMENDED to implement the same STAC version as the Item. A link with this rel type is required if the collection field in properties is present."""

    DERIVED_FROM = hydra.core.Name("derivedFrom")
    r"""URL to a STAC Item that was used as input data in the creation of this Item."""

StacRelationType.TYPE_ = hydra.core.Name("hydra.stac.items.StacRelationType")

class StacVersion(Node[str]):
    ...

StacVersion.TYPE_ = hydra.core.Name("hydra.stac.items.StacVersion")

class Uri(Node[str]):
    ...

Uri.TYPE_ = hydra.core.Name("hydra.stac.items.Uri")

class Url(Node[str]):
    ...

Url.TYPE_ = hydra.core.Name("hydra.stac.items.Url")
