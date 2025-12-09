module Hydra.Ext.Sources.Other.Atlas where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Additional imports
import qualified Hydra.Ext.Sources.Xml.Schema as XmlSchema


ns :: Namespace
ns = Namespace "hydra.ext.org.apache.atlas"

define :: String -> Type -> Binding
define = defineType ns

atlas :: String -> Type
atlas = typeref ns

xsd :: String -> Type
xsd = typeref (moduleNamespace XmlSchema.module_)

module_ :: Module
module_ = Module ns elements [XmlSchema.module_] [] $
    Just ("The Apache Atlas meta-model\n" ++
      "Based on the the org.apache.atlas.model package in the master branch as of 2022-06-01\n" ++
      "  https://github.com/apache/atlas/tree/master/intg/src/main/java/org/apache/atlas/model")
  where
    elements = [
      atlasAttribute,
      atlasAttributeDef_Cardinality,
      atlasAttributeDef_IndexType,
      atlasBaseType,
      atlasConstraint,
      atlasEntity,
      atlasRelationshipAttribute,
      atlasStruct,
      typeCategory]

atlasAttribute :: Binding
atlasAttribute = define "AtlasAttributeDef" $
  doc "class that captures details of a struct-attribute." $
  T.record [
    "name">: T.maybe T.string,
    "typeName">: T.maybe T.string,
    "isOptional">: T.boolean,
    "cardinality">: T.maybe $ atlas "AtlasAttributeDef_Cardinality",
    "valuesMinCount">: T.int32,
    "valuesMaxCount">: T.int32,
    "isUnique">: T.boolean,
    "isIndexable">: T.boolean,
    "includeInNotification">: T.boolean,
    "defaultValue">: T.maybe T.string,
    "description">: T.maybe T.string,
    "searchWeight">: T.int32,
    "indexType">: T.maybe $ atlas "AtlasAttributeDef_IndexType",
    "constraints">: T.list $ atlas "AtlasConstraintDef",
    "options">: T.map T.string T.string,
    "displayName">: T.maybe T.string]

atlasAttributeDef_Cardinality :: Binding
atlasAttributeDef_Cardinality = define "AtlasAttributeDef_Cardinality" $ T.enum ["single", "list", "set"]

atlasAttributeDef_IndexType :: Binding
atlasAttributeDef_IndexType = define "AtlasAttributeDef_IndexType" $ T.enum ["default", "string"]

atlasBaseType :: Binding
atlasBaseType = define "AtlasBaseTypeDef" $
  doc "Base class that captures common-attributes for all Atlas types." $
  T.record [
    "category">: T.maybe $ atlas "TypeCategory",
    "guid">: T.maybe T.string,
    "createdBy">: T.maybe T.string,
    "updatedBy">: T.maybe T.string,
    "createTime">: T.maybe $ xsd "DateTime",
    "updateTime">: T.maybe $ xsd "DateTime",
    "version">: T.maybe T.int64,
    "name">: T.maybe T.string,
    "description">: T.maybe T.string,
    "typeVersion">: T.maybe T.string,
    "serviceType">: T.maybe T.string,
    "options">: T.map T.string T.string]

atlasConstraint :: Binding
atlasConstraint = define "AtlasConstraintDef" $
  doc "class that captures details of a constraint." $
  T.record [
    "type">: T.maybe T.string,
    "params">: T.map T.string T.string] -- Map<String, Object>

atlasEntity :: Binding
atlasEntity = define "AtlasEntityDef" $
  doc "class that captures details of a entity-type." $
  T.record [
    "asAtlasStruct">: atlas "AtlasStructDef",

    "superTypes">:
      T.set T.string,

    "subTypes">:
      doc "the value of this field is derived from 'superTypes' specified in all AtlasEntityDef" $
      T.set T.string,

    "relationshipAttributeDefs">:
      doc "the value of this field is derived from all the relationshipDefs this entityType is referenced in" $
      T.list $ atlas "AtlasRelationshipAttributeDef",

    "businessAttributeDefs">:
      doc "the value of this field is derived from all the businessMetadataDefs this entityType is referenced in" $
      T.map T.string (T.list $ atlas "AtlasAttributeDef")]

atlasRelationshipAttribute :: Binding
atlasRelationshipAttribute = define "AtlasRelationshipAttributeDef" $
  doc "class that captures details of a struct-attribute." $
  T.record [
    "asAtlasAttribute">: atlas "AtlasAttributeDef",
    "relationshipTypeName">: T.maybe T.string,
    "isLegacyAttribute">: T.boolean]

atlasStruct :: Binding
atlasStruct = define "AtlasStructDef" $
  doc "class that captures details of a struct-type." $
  T.record [
    "asAtlasBaseType">: atlas "AtlasBaseTypeDef",

    "attributeDefs">: T.list $ atlas "AtlasAttributeDef"]

typeCategory :: Binding
typeCategory = define "TypeCategory" $ T.enum [
  "primitive", "objectIdType", "enum", "struct", "classification", "entity", "array", "map", "relationship", "businessMetadata"]
