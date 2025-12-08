module Hydra.Ext.Sources.Other.StacItems where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Additional imports
import qualified Hydra.Ext.Sources.Other.GeoJson as GeoJson
import qualified Hydra.Ext.Sources.Other.IanaRelations as IanaRelations


ns :: Namespace
ns = Namespace "hydra.ext.org.stacspec.items"

define :: String -> Type -> Binding
define = defineType ns

geoj :: String -> Type
geoj = typeref $ moduleNamespace GeoJson.module_

ianarel :: String -> Type
ianarel = typeref $ moduleNamespace IanaRelations.module_

stac :: String -> Type
stac = typeref ns

module_ :: Module
module_ = Module ns elements [GeoJson.module_, IanaRelations.module_] [Core.module_] $
    Just ("A model for SpatioTemporal Asset Catalog (STAC) Items. " ++
          "See https://github.com/radiantearth/stac-spec/blob/master/item-spec/item-spec.md")
  where
    elements = [
      asset,
      item,
      link,
      mediaType,
      relationType,
      role,
      stacRelationType,
      stacVersion,
      uri,
      url_]

asset :: Binding
asset = define "Asset" $
  doc ("An Asset is an object that contains a URI to data associated with the Item that can be downloaded " ++
       "or streamed. It is allowed to add additional fields.") $
  T.record [
    "href">:
      doc "URI to the asset object. Relative and absolute URI are both allowed." $
      stac "Uri",
    "title">:
      doc "The displayed title for clients and users." $
      T.maybe T.string,
    "description">:
      doc ("A description of the Asset providing additional details, such as how it was processed or " ++
           "created. CommonMark 0.29 syntax MAY be used for rich text representation.") $
      T.maybe T.string,
    "type">:
      doc ("Media type of the asset. See the common media types in the best practice doc for commonly " ++
           "used asset types.") $
      T.maybe $ stac "MediaType",
    "roles">:
      doc "The semantic roles of the asset, similar to the use of rel in links." $
      T.list $ stac "Role"]

item :: Binding
item = define "Item" $
  doc ("This object describes a STAC Item. The fields id, type, bbox, geometry and properties are inherited " ++
       "from GeoJSON.") $
  T.record [
    "feature">:
      geoj "Feature",
    "stacVersion">:
      doc "The STAC version the Item implements" $
      stac "StacVersion",
    "stacExtensions">:
      doc "A list of extensions the Item implements" $
      T.list $ stac "Url",
    "links">:
      doc ("List of link objects to resources and related URLs. A link with the rel set to self is strongly " ++
           "recommended.") $
      T.list $ stac "Link",
    "assets">:
      doc "Dictionary of asset objects that can be downloaded, each with a unique key." $
      T.map T.string (stac "Asset"),
    "collection">:
      doc ("The id of the STAC Collection this Item references to (see collection relation type). This field " ++
           "is required if such a relation type is present and is not allowed otherwise. This field provides " ++
           "an easy way for a user to search for any Items that belong in a specified Collection. Must be a " ++
           "non-empty string.") $
      T.maybe $ geoj "Id"]

link :: Binding
link = define "Link" $
  doc ("This object describes a relationship with another entity. Data providers are advised to be liberal " ++
       "with the links section, to describe things like the Catalog an Item is in, related Items, parent or " ++
       "child Items (modeled in different ways, like an 'acquisition' or derived data). It is allowed to add " ++
       "additional fields such as a title and type.") $
  T.record [
    "href">:
      doc "The actual link in the format of an URL. Relative and absolute links are both allowed." $
      stac "Url",
    "rel">:
      doc ("Relationship between the current document and the linked document. See chapter \"Relation types\" " ++
           "for more information.") $
      stac "RelationType",
    "type">:
      doc "Media type of the referenced entity." $
      T.maybe $ stac "MediaType",
    "title">:
      doc "A human readable title to be used in rendered displays of the link." $
      T.maybe T.string]

mediaType :: Binding
mediaType = define "MediaType" $ T.wrap T.string

relationType :: Binding
relationType = define "RelationType" $
  doc ("STAC Items use a variety of rel types in the link object, to describe the exact nature of the link " ++
       "between this Item and the entity it is linking to. It is recommended to use the official IANA Link " ++
       "Relation Types where possible. The following table explains places where STAC use custom rel types " ++
       "are used with Items. This happens where there is not a clear official option, or where STAC uses an " ++
       "official type but adds additional meaning for the STAC context.") $
  T.union [
    "iana">: ianarel "LinkRelationType",
    "stac">: stac "StacRelationType",
    "other">: T.string]

role :: Binding
role = define "Role" $
  doc ("The roles field is used to describe the purpose of each asset. It is recommended to include one for " ++
       "every asset, to give users a sense of why they might want to make use of the asset. There are some " ++
       "emerging standards that enable clients to take particular action when they encounter particular " ++
       "roles, listed below. But implementors are encouraged to come up with their own terms to describe the " ++
       "role.") $
  T.union [
    "thumbnail">:
      doc ("An asset that represents a thumbnail of the Item, typically a true color image (for Items with " ++
           "assets in the visible wavelengths), lower-resolution (typically smaller 600x600 pixels), and " ++
           "typically a JPEG or PNG (suitable for display in a web browser). Multiple assets may have this " ++
           "purpose, but it recommended that the type and roles be unique tuples. For example, Sentinel-2 L2A " ++
           "provides thumbnail images in both JPEG and JPEG2000 formats, and would be distinguished by their " ++
           "media types.") T.unit,
    "overview">:
      doc ("An asset that represents a possibly larger view than the thumbnail of the Item, for example, a " ++
           "true color composite of multi-band data.") T.unit,
    "data">:
      doc ("The data itself. This is a suggestion for a common role for data files to be used in case data " ++
           "providers don't come up with their own names and semantics.") T.unit,
    "metadata">:
      doc "A metadata sidecar file describing the data in this Item, for example the Landsat-8 MTL file." T.unit,
    "other">:
      T.string]

stacRelationType :: Binding
stacRelationType = define "StacRelationType" $
  T.union [
    "self">:
      doc ("STRONGLY RECOMMENDED. Absolute URL to the Item if it is available at a public URL. This is " ++
           "particularly useful when in a download package that includes metadata, so that the downstream " ++
           "user can know where the data has come from.") T.unit,
    "root">:
      doc "URL to the root STAC entity (Catalog or Collection)." T.unit,
    "parent">:
      doc ("URL to the parent STAC entity (Catalog or Collection).") T.unit,
    "collection">:
      doc ("STRONGLY RECOMMENDED. URL to a Collection. Absolute URLs should be used whenever possible. " ++
           "The referenced Collection is STRONGLY RECOMMENDED to implement the same STAC version as the Item. " ++
           "A link with this rel type is required if the collection field in properties is present.") T.unit,
    "derivedFrom">:
      doc ("URL to a STAC Item that was used as input data in the creation of this Item.") T.unit]

stacVersion :: Binding
stacVersion = define "StacVersion" $ T.wrap T.string

uri :: Binding
uri = define "Uri" $ T.wrap T.string

url_ :: Binding
url_ = define "Url" $ T.wrap T.string
