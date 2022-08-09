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
    def = datatype coreContext atlasModelName
    atlas = nsref atlasModelName
    xsd = nsref xmlSchemaName

    elements = [

      def "AtlasAttributeDef" $
        doc "class that captures details of a struct-attribute." $
        record [
          "name">: optional string,
          "typeName">: optional string,
          "isOptional">: boolean,
          "cardinality">: optional $ atlas "AtlasAttributeDef.Cardinality",
          "valuesMinCount">: int32,
          "valuesMaxCount">: int32,
          "isUnique">: boolean,
          "isIndexable">: boolean,
          "includeInNotification">: boolean,
          "defaultValue">: optional string,
          "description">: optional string,
          "searchWeight">: int32,
          "indexType">: optional $ atlas "AtlasAttributeDef.IndexType",
          "constraints">: list $ atlas "AtlasConstraintDef",
          "options">: Types.map string string,
          "displayName">: optional string],
            
      def "AtlasAttributeDef.Cardinality" $ enum ["single", "list", "set"],
      
      def "AtlasAttributeDef.IndexType" $ enum ["default", "string"],
        
      def "AtlasBaseTypeDef" $
        doc "Base class that captures common-attributes for all Atlas types." $
        record [
          "category">: optional $ atlas "TypeCategory",
          "guid">: optional string,
          "createdBy">: optional string,
          "updatedBy">: optional string,
          "createTime">: optional $ xsd "DateTime",
          "updateTime">: optional $ xsd "DateTime",
          "version">: optional int64,
          "name">: optional string,
          "description">: optional string,
          "typeVersion">: optional string,
          "serviceType">: optional string,
          "options">: Types.map string string],

      def "AtlasConstraintDef" $
        doc "class that captures details of a constraint." $
        record [
          "type">: optional string,
          "params">: Types.map string string], -- Map<String, Object>
      
      def "AtlasEntityDef" $
        doc "class that captures details of a entity-type." $
        record [
          "asAtlasStructDef">: atlas "AtlasStructDef",
          
          "superTypes">:
            Types.set string,
            
          "subTypes">:
            doc "the value of this field is derived from 'superTypes' specified in all AtlasEntityDef" $
            Types.set string,
            
          "relationshipAttributeDefs">:
            doc "the value of this field is derived from all the relationshipDefs this entityType is referenced in" $
            list $ atlas "AtlasRelationshipAttributeDef",
            
          "businessAttributeDefs">:
            doc "the value of this field is derived from all the businessMetadataDefs this entityType is referenced in" $
            Types.map string (list $ atlas "AtlasAttributeDef")],
            
        def "AtlasRelationshipAttributeDef" $
          doc "class that captures details of a struct-attribute." $
          record [
            "asAtlasAttributeDef">: atlas "AtlasAttributeDef",
            "relationshipTypeName">: optional string,
            "isLegacyAttribute">: boolean],

        def "AtlasStructDef" $
          doc "class that captures details of a struct-type." $
          record [
            "asAtlasBaseTypeDef">: atlas "AtlasBaseTypeDef",
           
            "attributeDefs">: list $ atlas "AtlasAttributeDef"],
            
        def "TypeCategory" $ enum [
          "primitive", "objectIdType", "enum", "struct", "classification", "entity", "array", "map", "relationship", "businessMetadata"]]
