{-|
Model for Apache Atlas meta-model

Based on the the org.apache.atlas.model package in the master branch as of 2022-06-01
  https://github.com/apache/atlas/tree/master/intg/src/main/java/org/apache/atlas/model
-}

module Hydra.Impl.Haskell.Sources.Ext.Atlas.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Ext.Xml.Schema


atlasModelModule :: Module Meta
atlasModelModule = Module atlasModel [xmlSchemaModule]

atlasModelName :: GraphName
atlasModelName = GraphName "hydra/ext/atlas/model"

atlasModel :: Graph Meta
atlasModel = Graph atlasModelName elements (const True) hydraCoreName
  where
    def = datatype atlasModelName
    atlas = nsref atlasModelName
    xsd = nsref xmlSchemaName

    elements = [

      def "AtlasAttributeDef" $
        doc "class that captures details of a struct-attribute." $
        record [
          field "name" $ optional string,
          field "typeName" $ optional string,
          field "isOptional" boolean,
          field "cardinality" $ optional $ atlas "AtlasAttributeDef.Cardinality",
          field "valuesMinCount" int32,
          field "valuesMaxCount" int32,
          field "isUnique" boolean,
          field "isIndexable" boolean,
          field "includeInNotification" boolean,
          field "defaultValue" $ optional string,
          field "description" $ optional string,
          field "searchWeight" int32,
          field "indexType" $ optional $ atlas "AtlasAttributeDef.IndexType",
          field "constraints" $ list $ atlas "AtlasConstraintDef",
          field "options" $ Types.map string string,
          field "displayName" $ optional string],
            
      def "AtlasAttributeDef.Cardinality" $ enum ["single", "list", "set"],
      
      def "AtlasAttributeDef.IndexType" $ enum ["default", "string"],
        
      def "AtlasBaseTypeDef" $
        doc "Base class that captures common-attributes for all Atlas types." $
        record [
          field "category" $ optional $ atlas "TypeCategory",
          field "guid" $ optional string,
          field "createdBy" $ optional string,
          field "updatedBy" $ optional string,
          field "createTime" $ optional $ xsd "DateTime",
          field "updateTime" $ optional $ xsd "DateTime",
          field "version" $ optional int64,
          field "name" $ optional string,
          field "description" $ optional string,
          field "typeVersion" $ optional string,
          field "serviceType" $ optional string,
          field "options" $ Types.map string string],

      def "AtlasConstraintDef" $
        doc "class that captures details of a constraint." $
        record [
          field "type" $ optional string,
          field "params" $ Types.map string string], -- Map<String, Object>
      
      def "AtlasEntityDef" $
        doc "class that captures details of a entity-type." $
        record [
          field "asAtlasStructDef" $ atlas "AtlasStructDef",
          
          field "superTypes" $
            Types.set string,
            
          field "subTypes" $
            doc "the value of this field is derived from 'superTypes' specified in all AtlasEntityDef" $
            Types.set string,
            
          field "relationshipAttributeDefs" $
            doc "the value of this field is derived from all the relationshipDefs this entityType is referenced in" $
            list $ atlas "AtlasRelationshipAttributeDef",
            
          field "businessAttributeDefs" $
            doc "the value of this field is derived from all the businessMetadataDefs this entityType is referenced in" $
            Types.map string (list $ atlas "AtlasAttributeDef")],
            
        def "AtlasRelationshipAttributeDef" $
          doc "class that captures details of a struct-attribute." $
          record [
            field "asAtlasAttributeDef" $ atlas "AtlasAttributeDef",
            field "relationshipTypeName" $ optional string,
            field "isLegacyAttribute" boolean],

        def "AtlasStructDef" $
          doc "class that captures details of a struct-type." $
          record [
            field "asAtlasBaseTypeDef" $ atlas "AtlasBaseTypeDef",
           
            field "attributeDefs" $ list $ atlas "AtlasAttributeDef"],
            
        def "TypeCategory" $ enum [
          "primitive", "objectIdType", "enum", "struct", "classification", "entity", "array", "map", "relationship", "businessMetadata"]]
