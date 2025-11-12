module Hydra.Ext.Sources.Other.Atlas where

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

-- Additional imports
import Hydra.Ext.Sources.Xml.Schema


atlasModelModule :: Module
atlasModelModule = Module ns elements [xmlSchemaModule] [] $
    Just ("The Apache Atlas meta-model\n" ++
      "Based on the the org.apache.atlas.model package in the master branch as of 2022-06-01\n" ++
      "  https://github.com/apache/atlas/tree/master/intg/src/main/java/org/apache/atlas/model")
  where
    ns = Namespace "hydra.ext.org.apache.atlas"
    def = datatype ns
    atlas = typeref ns
    xsd = typeref (moduleNamespace xmlSchemaModule)

    elements = [

      def "AtlasAttributeDef" $
        doc "class that captures details of a struct-attribute." $
        record [
          "name">: optional string,
          "typeName">: optional string,
          "isOptional">: boolean,
          "cardinality">: optional $ atlas "AtlasAttributeDef_Cardinality",
          "valuesMinCount">: int32,
          "valuesMaxCount">: int32,
          "isUnique">: boolean,
          "isIndexable">: boolean,
          "includeInNotification">: boolean,
          "defaultValue">: optional string,
          "description">: optional string,
          "searchWeight">: int32,
          "indexType">: optional $ atlas "AtlasAttributeDef_IndexType",
          "constraints">: list $ atlas "AtlasConstraintDef",
          "options">: Types.map string string,
          "displayName">: optional string],

      def "AtlasAttributeDef_Cardinality" $ enum ["single", "list", "set"],

      def "AtlasAttributeDef_IndexType" $ enum ["default", "string"],

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
