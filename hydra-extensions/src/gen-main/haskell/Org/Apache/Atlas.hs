-- | The Apache Atlas meta-model
-- | Based on the the org.apache.atlas.model package in the master branch as of 2022-06-01
-- |   https://github.com/apache/atlas/tree/master/intg/src/main/java/org/apache/atlas/model

module Org.Apache.Atlas where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Xml.Schema as Schema
import Data.List
import Data.Map
import Data.Set

-- | class that captures details of a struct-attribute.
data AtlasAttributeDef = 
  AtlasAttributeDef {
    atlasAttributeDefName :: (Maybe String),
    atlasAttributeDefTypeName :: (Maybe String),
    atlasAttributeDefIsOptional :: Bool,
    atlasAttributeDefCardinality :: (Maybe AtlasAttributeDef_Cardinality),
    atlasAttributeDefValuesMinCount :: Int,
    atlasAttributeDefValuesMaxCount :: Int,
    atlasAttributeDefIsUnique :: Bool,
    atlasAttributeDefIsIndexable :: Bool,
    atlasAttributeDefIncludeInNotification :: Bool,
    atlasAttributeDefDefaultValue :: (Maybe String),
    atlasAttributeDefDescription :: (Maybe String),
    atlasAttributeDefSearchWeight :: Int,
    atlasAttributeDefIndexType :: (Maybe AtlasAttributeDef_IndexType),
    atlasAttributeDefConstraints :: [AtlasConstraintDef],
    atlasAttributeDefOptions :: (Map String String),
    atlasAttributeDefDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_AtlasAttributeDef = (Core.Name "org/apache/atlas.AtlasAttributeDef")

_AtlasAttributeDef_name = (Core.FieldName "name")

_AtlasAttributeDef_typeName = (Core.FieldName "typeName")

_AtlasAttributeDef_isOptional = (Core.FieldName "isOptional")

_AtlasAttributeDef_cardinality = (Core.FieldName "cardinality")

_AtlasAttributeDef_valuesMinCount = (Core.FieldName "valuesMinCount")

_AtlasAttributeDef_valuesMaxCount = (Core.FieldName "valuesMaxCount")

_AtlasAttributeDef_isUnique = (Core.FieldName "isUnique")

_AtlasAttributeDef_isIndexable = (Core.FieldName "isIndexable")

_AtlasAttributeDef_includeInNotification = (Core.FieldName "includeInNotification")

_AtlasAttributeDef_defaultValue = (Core.FieldName "defaultValue")

_AtlasAttributeDef_description = (Core.FieldName "description")

_AtlasAttributeDef_searchWeight = (Core.FieldName "searchWeight")

_AtlasAttributeDef_indexType = (Core.FieldName "indexType")

_AtlasAttributeDef_constraints = (Core.FieldName "constraints")

_AtlasAttributeDef_options = (Core.FieldName "options")

_AtlasAttributeDef_displayName = (Core.FieldName "displayName")

data AtlasAttributeDef_Cardinality = 
  AtlasAttributeDef_CardinalitySingle  |
  AtlasAttributeDef_CardinalityList  |
  AtlasAttributeDef_CardinalitySet 
  deriving (Eq, Ord, Read, Show)

_AtlasAttributeDef_Cardinality = (Core.Name "org/apache/atlas.AtlasAttributeDef.Cardinality")

_AtlasAttributeDef_Cardinality_single = (Core.FieldName "single")

_AtlasAttributeDef_Cardinality_list = (Core.FieldName "list")

_AtlasAttributeDef_Cardinality_set = (Core.FieldName "set")

data AtlasAttributeDef_IndexType = 
  AtlasAttributeDef_IndexTypeDefault  |
  AtlasAttributeDef_IndexTypeString 
  deriving (Eq, Ord, Read, Show)

_AtlasAttributeDef_IndexType = (Core.Name "org/apache/atlas.AtlasAttributeDef.IndexType")

_AtlasAttributeDef_IndexType_default = (Core.FieldName "default")

_AtlasAttributeDef_IndexType_string = (Core.FieldName "string")

-- | Base class that captures common-attributes for all Atlas types.
data AtlasBaseTypeDef = 
  AtlasBaseTypeDef {
    atlasBaseTypeDefCategory :: (Maybe TypeCategory),
    atlasBaseTypeDefGuid :: (Maybe String),
    atlasBaseTypeDefCreatedBy :: (Maybe String),
    atlasBaseTypeDefUpdatedBy :: (Maybe String),
    atlasBaseTypeDefCreateTime :: (Maybe Schema.DateTime),
    atlasBaseTypeDefUpdateTime :: (Maybe Schema.DateTime),
    atlasBaseTypeDefVersion :: (Maybe Integer),
    atlasBaseTypeDefName :: (Maybe String),
    atlasBaseTypeDefDescription :: (Maybe String),
    atlasBaseTypeDefTypeVersion :: (Maybe String),
    atlasBaseTypeDefServiceType :: (Maybe String),
    atlasBaseTypeDefOptions :: (Map String String)}
  deriving (Eq, Ord, Read, Show)

_AtlasBaseTypeDef = (Core.Name "org/apache/atlas.AtlasBaseTypeDef")

_AtlasBaseTypeDef_category = (Core.FieldName "category")

_AtlasBaseTypeDef_guid = (Core.FieldName "guid")

_AtlasBaseTypeDef_createdBy = (Core.FieldName "createdBy")

_AtlasBaseTypeDef_updatedBy = (Core.FieldName "updatedBy")

_AtlasBaseTypeDef_createTime = (Core.FieldName "createTime")

_AtlasBaseTypeDef_updateTime = (Core.FieldName "updateTime")

_AtlasBaseTypeDef_version = (Core.FieldName "version")

_AtlasBaseTypeDef_name = (Core.FieldName "name")

_AtlasBaseTypeDef_description = (Core.FieldName "description")

_AtlasBaseTypeDef_typeVersion = (Core.FieldName "typeVersion")

_AtlasBaseTypeDef_serviceType = (Core.FieldName "serviceType")

_AtlasBaseTypeDef_options = (Core.FieldName "options")

-- | class that captures details of a constraint.
data AtlasConstraintDef = 
  AtlasConstraintDef {
    atlasConstraintDefType :: (Maybe String),
    atlasConstraintDefParams :: (Map String String)}
  deriving (Eq, Ord, Read, Show)

_AtlasConstraintDef = (Core.Name "org/apache/atlas.AtlasConstraintDef")

_AtlasConstraintDef_type = (Core.FieldName "type")

_AtlasConstraintDef_params = (Core.FieldName "params")

-- | class that captures details of a entity-type.
data AtlasEntityDef = 
  AtlasEntityDef {
    atlasEntityDefAsAtlasStructDef :: AtlasStructDef,
    atlasEntityDefSuperTypes :: (Set String),
    -- | the value of this field is derived from 'superTypes' specified in all AtlasEntityDef
    atlasEntityDefSubTypes :: (Set String),
    -- | the value of this field is derived from all the relationshipDefs this entityType is referenced in
    atlasEntityDefRelationshipAttributeDefs :: [AtlasRelationshipAttributeDef],
    -- | the value of this field is derived from all the businessMetadataDefs this entityType is referenced in
    atlasEntityDefBusinessAttributeDefs :: (Map String [AtlasAttributeDef])}
  deriving (Eq, Ord, Read, Show)

_AtlasEntityDef = (Core.Name "org/apache/atlas.AtlasEntityDef")

_AtlasEntityDef_asAtlasStructDef = (Core.FieldName "asAtlasStructDef")

_AtlasEntityDef_superTypes = (Core.FieldName "superTypes")

_AtlasEntityDef_subTypes = (Core.FieldName "subTypes")

_AtlasEntityDef_relationshipAttributeDefs = (Core.FieldName "relationshipAttributeDefs")

_AtlasEntityDef_businessAttributeDefs = (Core.FieldName "businessAttributeDefs")

-- | class that captures details of a struct-attribute.
data AtlasRelationshipAttributeDef = 
  AtlasRelationshipAttributeDef {
    atlasRelationshipAttributeDefAsAtlasAttributeDef :: AtlasAttributeDef,
    atlasRelationshipAttributeDefRelationshipTypeName :: (Maybe String),
    atlasRelationshipAttributeDefIsLegacyAttribute :: Bool}
  deriving (Eq, Ord, Read, Show)

_AtlasRelationshipAttributeDef = (Core.Name "org/apache/atlas.AtlasRelationshipAttributeDef")

_AtlasRelationshipAttributeDef_asAtlasAttributeDef = (Core.FieldName "asAtlasAttributeDef")

_AtlasRelationshipAttributeDef_relationshipTypeName = (Core.FieldName "relationshipTypeName")

_AtlasRelationshipAttributeDef_isLegacyAttribute = (Core.FieldName "isLegacyAttribute")

-- | class that captures details of a struct-type.
data AtlasStructDef = 
  AtlasStructDef {
    atlasStructDefAsAtlasBaseTypeDef :: AtlasBaseTypeDef,
    atlasStructDefAttributeDefs :: [AtlasAttributeDef]}
  deriving (Eq, Ord, Read, Show)

_AtlasStructDef = (Core.Name "org/apache/atlas.AtlasStructDef")

_AtlasStructDef_asAtlasBaseTypeDef = (Core.FieldName "asAtlasBaseTypeDef")

_AtlasStructDef_attributeDefs = (Core.FieldName "attributeDefs")

data TypeCategory = 
  TypeCategoryPrimitive  |
  TypeCategoryObjectIdType  |
  TypeCategoryEnum  |
  TypeCategoryStruct  |
  TypeCategoryClassification  |
  TypeCategoryEntity  |
  TypeCategoryArray  |
  TypeCategoryMap  |
  TypeCategoryRelationship  |
  TypeCategoryBusinessMetadata 
  deriving (Eq, Ord, Read, Show)

_TypeCategory = (Core.Name "org/apache/atlas.TypeCategory")

_TypeCategory_primitive = (Core.FieldName "primitive")

_TypeCategory_objectIdType = (Core.FieldName "objectIdType")

_TypeCategory_enum = (Core.FieldName "enum")

_TypeCategory_struct = (Core.FieldName "struct")

_TypeCategory_classification = (Core.FieldName "classification")

_TypeCategory_entity = (Core.FieldName "entity")

_TypeCategory_array = (Core.FieldName "array")

_TypeCategory_map = (Core.FieldName "map")

_TypeCategory_relationship = (Core.FieldName "relationship")

_TypeCategory_businessMetadata = (Core.FieldName "businessMetadata")