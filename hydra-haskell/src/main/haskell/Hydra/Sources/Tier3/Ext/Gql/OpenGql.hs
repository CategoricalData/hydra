{-# LANGUAGE OverloadedStrings #-}
module Hydra.Sources.Tier3.Ext.Gql.OpenGql where

import Hydra.Kernel
import qualified Hydra.Sources.Tier1.All as Tier1
import qualified Hydra.Sources.Tier2.All as Tier2
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

openGqlModule :: Module
openGqlModule = Module ns elements [Tier1.hydraCoreModule] [Tier1.hydraCoreModule]
  $ Just ("A GQL model based on the OpenGQL ANTLR grammar, version 15b256b (2024-09-05), available at:"
    ++ " https://github.com/opengql/grammar/blob/main/GQL.g4")
  where
    ns = Namespace "openGql.grammar"
    def = datatype ns
    gql = typeref ns

    elements = [

-- grammar GQL;
--
-- options { caseInsensitive = true; }
--
-- // 6 <GQL-program>
--
-- gqlProgram
--     : programActivity sessionCloseCommand? EOF
--     | sessionCloseCommand EOF
--     ;
      def "GqlProgram" $
        record [
          "activity">: optional $ gql "ProgramActivity",
          "close">: optional $ gql "SessionCloseCommand"],

-- programActivity = sessionActivity
--                  | transactionActivity
--                  ;
      def "ProgramActivity" $
        union [
          "session">: gql "SessionActivity",
          "transaction">: gql "TransactionActivity"],

-- sessionActivity = sessionResetCommand+
--                 | sessionSetCommand+ sessionResetCommand*
--                 ;
      def "SessionActivity" $
        union [
          "reset">: nonemptyList $ gql "SessionResetCommand",
          "setAndResetCommands">: gql "SessionSetAndResetCommands"],

      def "SessionSetAndResetCommands" $
        record [
          "set">: nonemptyList $ gql "SessionSetCommand",
          "reset">: list $ gql "SessionResetCommand"],

-- transactionActivity = startTransactionCommand (procedureSpecification endTransactionCommand?)?
--                     | procedureSpecification endTransactionCommand?
--                     | endTransactionCommand
--                     ;
      def "TransactionActivity" $
        union [
          "start">: gql "StartAndMaybeProcedureAndMaybeEnd",
          "procedure">: gql "ProcedureAndMaybeEnd",
          "end">: gql "EndTransactionCommand"],

      def "StartAndMaybeProcedureAndMaybeEnd" $
        record [
          "start">: gql "StartTransactionCommand",
          "procedureAndEnd">: optional $ gql "ProcedureAndMaybeEnd"],

      def "ProcedureAndMaybeEnd" $
        record [
          "procedure">: gql "ProcedureSpecification",
          "end">: optional $ gql "EndTransactionCommand"],

-- endTransactionCommand = rollbackCommand
--                       | commitCommand
--                       ;
      def "EndTransactionCommand" $
        union [
          "rollback">: gql "RollbackCommand",
          "commit">: gql "CommitCommand"],

-- // 7.1 <session set command>
--
-- sessionSetCommand = SESSION SET (sessionSetSchemaClause | sessionSetGraphClause | sessionSetTimeZoneClause | sessionSetParameterClause)
--                   ;
      def "SessionSetCommand" $
        union [
          "schema">: gql "SessionSetSchemaClause",
          "graph">: gql "SessionSetGraphClause",
          "timeZone">: gql "SessionSetTimeZoneClause",
          "parameter">: gql "SessionSetParameterClause"],

-- sessionSetSchemaClause = SCHEMA schemaReference
--                        ;
      def "SessionSetSchemaClause" $
        gql "SchemaReference",

-- sessionSetGraphClause = PROPERTY? GRAPH graphExpression
--                       ;
      def "SessionSetGraphClause" $
        gql "GraphExpression",

-- sessionSetTimeZoneClause = TIME ZONE setTimeZoneValue
--                          ;
      def "SessionSetTimeZoneClause" $
        gql "SetTimeZoneValue",

-- setTimeZoneValue = timeZoneString
--                  ;
      def "SetTimeZoneValue" $
        gql "TimeZoneString",

-- sessionSetParameterClause = sessionSetGraphParameterClause
--                           | sessionSetBindingTableParameterClause
--                           | sessionSetValueParameterClause
--                           ;
      def "SessionSetParameterClause" $
        union [
          "graph">: gql "SessionSetGraphParameterClause",
          "bindings">: gql "SessionSetBindingTableParameterClause",
          "value">: gql "SessionSetValueParameterClause"],

-- sessionSetGraphParameterClause = PROPERTY? GRAPH sessionSetParameterName optTypedGraphInitializer
--                                ;
      def "SessionSetGraphParameterClause" $
        record [
          "graph">: gql "SessionSetParameterName",
          "initializer">: gql "OptTypedGraphInitializer"],

-- sessionSetBindingTableParameterClause = BINDING? TABLE sessionSetParameterName optTypedBindingTableInitializer
--                                       ;
      def "SessionSetBindingTableParameterClause" $
        record [
          "binding">: boolean,
          "param">: gql "SessionSetParameterName",
          "init">: gql "OptTypedBindingTableInitializer"],

-- sessionSetValueParameterClause = VALUE sessionSetParameterName optTypedValueInitializer
--                                ;
      def "SessionSetValueParameterClause" $
        record [
          "value">: gql "SessionSetParameterName",
          "initializer">: gql "OptTypedValueInitializer"],

-- sessionSetParameterName = (IF NOT EXISTS)? sessionParameterSpecification
--                         ;
      def "SessionSetParameterName" $
        record [
          "ifNotExists">: boolean,
          "parameter">: gql "SessionParameterSpecification"],

-- // 7.2 <session reset command>
--
-- sessionResetCommand
--     : SESSION RESET sessionResetArguments?
--     ;
      def "SessionResetCommand" $
        optional $ gql "SessionResetArguments",

-- sessionResetArguments
--     : ALL? (PARAMETERS | CHARACTERISTICS)
--     | SCHEMA
--     | PROPERTY? GRAPH
--     | TIME ZONE
--     | PARAMETER? sessionParameterSpecification
--     ;
      def "SessionResetArguments" $
        union [
          "parametersOrCharacteristics">: gql "AllParametersOrCharacteristics",
          "schema">: unit,
          "graph">: unit,
          "timeZone">: unit,
          "parameterSessionSpecification">: gql "ParameterSessionSpecification"],

      def "AllParametersOrCharacteristics" $
        record [
          "all">: boolean,
          "type">: gql "ParametersOrCharacteristics"],

      def "ParametersOrCharacteristics" $
        enum ["parameters", "characteristics"],

      def "ParameterSessionSpecification" $
        record [
          "parameter">: boolean,
          "sessionParameterSpecification">: gql "SessionParameterSpecification"],

-- // 7.3 <session close command>
--
-- sessionCloseCommand
--     : SESSION CLOSE
--     ;
      def "SessionCloseCommand"
        unit,

-- // 7.4 <session parameter specification>
--
-- sessionParameterSpecification
--     : GENERAL_PARAMETER_REFERENCE
--     ;
      def "SessionParameterSpecification" $
        gql "ParameterName",

-- // 8.1 <start transaction command>
--
-- startTransactionCommand
--     : START TRANSACTION transactionCharacteristics?
--     ;
      def "StartTransactionCommand" $
        optional $ gql "TransactionCharacteristics",

-- // 8.2 <transaction characteristics>

-- transactionCharacteristics
--     : transactionMode (COMMA transactionMode)*
--     ;
      def "TransactionCharacteristics" $
        nonemptyList $ gql "TransactionMode",

-- transactionMode
--     : transactionAccessMode
--     ;
      def "TransactionMode" $
        gql "TransactionAccessMode",

-- transactionAccessMode
--     : READ ONLY
--     | READ WRITE
--     ;
      def "TransactionAccessMode" $
        enum [
          "readOnly",
          "readWrite"],

-- // 8.3 <rollback command>

-- rollbackCommand
--     : ROLLBACK
--     ;
      def "RollbackCommand"
        unit,

-- // 8.4 <commit command>

-- commitCommand
--     : COMMIT
--     ;
      def "CommitCommand"
        unit,

-- // 9.1 <nested procedure specification>

-- nestedProcedureSpecification
--     : LEFT_BRACE procedureSpecification RIGHT_BRACE
--     ;
      def "NestedProcedureSpecification" $
        gql "ProcedureSpecification",

-- procedureSpecification
--     : procedureBody
-- //    : catalogModifyingProcedureSpecification
-- //    | dataModifyingProcedureSpecification
-- //    | querySpecification
--     ;
      def "ProcedureSpecification" $
        gql "ProcedureBody",

-- //catalogModifyingProcedureSpecification
-- //    : procedureBody
-- //    ;

-- nestedDataModifyingProcedureSpecification
--     : LEFT_BRACE procedureBody RIGHT_BRACE
--     ;
      def "NestedDataModifyingProcedureSpecification" $
        gql "ProcedureBody",

-- //dataModifyingProcedureSpecification
-- //    : procedureBody
-- //    ;

-- nestedQuerySpecification
--     : LEFT_BRACE procedureBody RIGHT_BRACE
--     ;
      def "NestedQuerySpecification" $
        gql "ProcedureBody",

-- //querySpecification
-- //    : procedureBody
-- //    ;

-- // 9.2 <procedure body>

-- procedureBody
--     : atSchemaClause? bindingVariableDefinitionBlock? statementBlock
--     ;
      def "ProcedureBody" $
        record [
          "atSchema">: optional $ gql "AtSchemaClause",
          "bindings">: optional $ gql "BindingVariableDefinitionBlock",
          "statements">: gql "StatementBlock"],

-- bindingVariableDefinitionBlock
--     : bindingVariableDefinition+
--     ;
      def "BindingVariableDefinitionBlock" $
        nonemptyList $ gql "BindingVariableDefinition",

-- bindingVariableDefinition
--     : graphVariableDefinition
--     | bindingTableVariableDefinition
--     | valueVariableDefinition
--     ;
      def "BindingVariableDefinition" $
        union [
          "graph">: gql "GraphVariableDefinition",
          "table">: gql "BindingTableVariableDefinition",
          "value">: gql "ValueVariableDefinition"],

-- statementBlock
--     : statement nextStatement*
--     ;
      def "StatementBlock" $
        record [
          "statement">: gql "Statement",
          "nextStatements">: list $ gql "NextStatement"],

-- statement
--     : linearCatalogModifyingStatement
--     | linearDataModifyingStatement
--     | compositeQueryStatement
--     ;
      def "Statement" $
        union [
          "linearCatalogModifying">: gql "LinearCatalogModifyingStatement",
          "linearDataModifying">: gql "LinearDataModifyingStatement",
          "compositeQuery">: gql "CompositeQueryStatement"],

-- nextStatement
--     : NEXT yieldClause? statement
--     ;
      def "NextStatement" $
        record [
          "yieldClause">: optional $ gql "YieldClause",
          "statement">: gql "Statement"],

-- // 10.1 <graph variable definition>

-- graphVariableDefinition
--     : PROPERTY? GRAPH bindingVariable optTypedGraphInitializer
--     ;
      def "GraphVariableDefinition" $
        record [
          "variable">: gql "BindingVariable",
          "initializer">: gql "OptTypedGraphInitializer"],

-- optTypedGraphInitializer
--     : (typed? graphReferenceValueType)? graphInitializer
--     ;
      def "OptTypedGraphInitializer" $
        record [
          "type">: optional $ gql "TypedGraphReferenceValueType",
          "initializer">: gql "GraphInitializer"],

      def "TypedGraphReferenceValueType" $
        record [
          "typed">: optional $ gql "Typed",
          "valueType">: gql "GraphReferenceValueType"],

-- graphInitializer
--     : EQUALS_OPERATOR graphExpression
--     ;
      def "GraphInitializer" $ gql "GraphExpression",

-- // 10.2 <binding table variable definition>

-- bindingTableVariableDefinition
--     : BINDING? TABLE bindingVariable optTypedBindingTableInitializer
--     ;
      def "BindingTableVariableDefinition" $
        record [
          "binding">: boolean,
          "variable">: gql "BindingVariable",
          "initializer">: gql "OptTypedBindingTableInitializer"],

-- optTypedBindingTableInitializer
--     : (typed? bindingTableReferenceValueType)? bindingTableInitializer
--     ;
      def "OptTypedBindingTableInitializer" $
        record [
          "type">: optional $ gql "TypedBindingTableReferenceValueType",
          "initializer">: gql "BindingTableInitializer"],

-- typedBindingTableReferenceValueType
      def "TypedBindingTableReferenceValueType" $
        record [
          "typed">: optional $ gql "Typed",
          "valueType">: gql "BindingTableReferenceValueType"],

-- bindingTableInitializer
--     : EQUALS_OPERATOR bindingTableExpression
--     ;
      def "BindingTableInitializer" $ gql "BindingTableExpression",

-- // 10.3 <value variable definition>

-- valueVariableDefinition
--     : VALUE bindingVariable optTypedValueInitializer
--     ;
      def "ValueVariableDefinition" $
        record [
          "variable">: gql "BindingVariable",
          "initializer">: gql "OptTypedValueInitializer"],

-- optTypedValueInitializer
--     : (typed? valueType)? valueInitializer
--     ;
      def "OptTypedValueInitializer" $
        record [
          "type">: optional $ gql "TypedValueType",
          "initializer">: gql "ValueInitializer"],

-- typedValueType
      def "TypedValueType" $
        record [
          "typed">: optional $ gql "Typed",
          "valueType">: gql "ValueType"],

-- valueInitializer
--     : EQUALS_OPERATOR valueExpression
--     ;
      def "ValueInitializer" $ gql "ValueExpression",

-- // 11.1 <graph expression>

-- graphExpression
--     : objectExpressionPrimary
--     | graphReference
--     | objectNameOrBindingVariable
--     | currentGraph
--     ;
      def "GraphExpression" $
        union [
          "object">: gql "ObjectExpressionPrimary",
          "reference">: gql "GraphReference",
          "name">: gql "ObjectNameOrBindingVariable",
          "current">: gql "CurrentGraph"],

-- currentGraph
--     : CURRENT_PROPERTY_GRAPH
--     | CURRENT_GRAPH
--     ;
      def "CurrentGraph" $
        enum ["graph", "propertyGraph"],

-- // 11.2 <binding table expression>

-- bindingTableExpression
--     : nestedBindingTableQuerySpecification
--     | objectExpressionPrimary
--     | bindingTableReference
--     | objectNameOrBindingVariable
--     ;
      def "BindingTableExpression" $
        union [
          "nested">: gql "NestedBindingTableQuerySpecification",
          "object">: gql "ObjectExpressionPrimary",
          "table">: gql "BindingTableReference",
          "name">: gql "ObjectNameOrBindingVariable"],

-- nestedBindingTableQuerySpecification
--     : nestedQuerySpecification
--     ;
      def "NestedBindingTableQuerySpecification" $ gql "NestedQuerySpecification",

-- // 11.3 <object expression primary>

-- objectExpressionPrimary
--     : VARIABLE valueExpressionPrimary
--     | parenthesizedValueExpression
--     | nonParenthesizedPrimaryValueExpressionSpecialCase
--     ;
      def "ObjectExpressionPrimary" $
        union [
          "variable">: gql "PrimaryValueExpression",
          "parenthesized">: gql "ParenthesizedValueExpression",
          "nonParenthesized">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase"],

-- // 12.1 <linear catalog-modifying statement>
--
-- linearCatalogModifyingStatement
--     : simpleCatalogModifyingStatement+
--     ;
      def "LinearCatalogModifyingStatement" $
        nonemptyList $ gql "SimpleCatalogModifyingStatement",

-- simpleCatalogModifyingStatement
--     : primitiveCatalogModifyingStatement
--     | callCatalogModifyingProcedureStatement
--     ;
      def "SimpleCatalogModifyingStatement" $
        union [
          "primitive">: gql "PrimitiveCatalogModifyingStatement",
          "callProcedure">: gql "CallCatalogModifyingProcedureStatement"],

-- primitiveCatalogModifyingStatement
--     : createSchemaStatement
--     | dropSchemaStatement
--     | createGraphStatement
--     | dropGraphStatement
--     | createGraphTypeStatement
--     | dropGraphTypeStatement
--     ;
      def "PrimitiveCatalogModifyingStatement" $
        union [
          "createSchema">: gql "CreateSchemaStatement",
          "dropSchema">: gql "DropSchemaStatement",
          "createGraph">: gql "CreateGraphStatement",
          "dropGraph">: gql "DropGraphStatement",
          "createGraphType">: gql "CreateGraphTypeStatement",
          "dropGraphType">: gql "DropGraphTypeStatement"],

-- // 12.2 <insert schema statement>
--
-- createSchemaStatement
--     : CREATE SCHEMA (IF NOT EXISTS)? catalogSchemaParentAndName
--     ;
      def "CreateSchemaStatement" $
        record [
          "ifNotExists">: boolean,
          "parentAndName">: gql "CatalogSchemaParentAndName"],

-- // 12.3 <drop schema statement>
--
-- dropSchemaStatement
--     : DROP SCHEMA (IF EXISTS)? catalogSchemaParentAndName
--     ;
      def "DropSchemaStatement" $
        record [
          "ifExists">: boolean,
          "parentAndName">: gql "CatalogSchemaParentAndName"],

-- // 12.4 <insert graph statement>
--
-- createGraphStatement
--     : CREATE (PROPERTY? GRAPH (IF NOT EXISTS)? | OR REPLACE PROPERTY? GRAPH) catalogGraphParentAndName (openGraphType | ofGraphType) graphSource?
--     ;
      def "CreateGraphStatement" $
        record [
          "createOption">: gql "CreateGraphOption",
          "parentAndName">: gql "CatalogGraphParentAndName",
          "type">: gql "GraphTypeOption",
          "source">: optional $ gql "GraphSource"],

      def "CreateGraphOption" $
        union [
          "graphIfNotExists">: boolean,
          "orReplace">: unit],

      def "GraphTypeOption" $
        union [
          "openGraphType">: gql "OpenGraphType",
          "ofGraphType">: gql "OfGraphType"],

-- openGraphType
--     : typed? ANY (PROPERTY? GRAPH)?
--     ;
      def "OpenGraphType" $
        record [
          "typed">: optional $ gql "Typed",
          "graph">: boolean],

-- ofGraphType
--     : graphTypeLikeGraph
--     | typed? graphTypeReference
--     | typed? (PROPERTY? GRAPH)? nestedGraphTypeSpecification
--     ;
      def "OfGraphType" $
        union [
          "likeGraph">: gql "GraphTypeLikeGraph",
          "reference">: gql "TypedGraphTypeReference",
          "nested">: gql "TypedNestedGraphTypeSpecification"],

-- graphTypeLikeGraph
--     : LIKE graphExpression
--     ;
      def "GraphTypeLikeGraph" $
        gql "GraphExpression",

-- graphSource
--     : AS COPY OF graphExpression
--     ;
      def "GraphSource" $
        gql "GraphExpression",

-- typed? graphTypeReference
      def "TypedGraphTypeReference" $
        record [
          "typed">: optional $ gql "Typed",
          "reference">: gql "GraphTypeReference"],

-- typed? (PROPERTY? GRAPH)? nestedGraphTypeSpecification
      def "TypedNestedGraphTypeSpecification" $
        record [
          "typed">: optional $ gql "Typed",
          "graph">: boolean,
          "specification">: gql "NestedGraphTypeSpecification"],

-- // 12.5 <drop graph statement>
--
-- dropGraphStatement
--     : DROP PROPERTY? GRAPH (IF EXISTS)? catalogGraphParentAndName
--     ;
      def "DropGraphStatement" $
        record [
          "ifExists">: boolean,
          "parentAndName">: gql "CatalogGraphParentAndName"],

-- // 12.6 <graph type statement>
--
-- createGraphTypeStatement
--     : CREATE (PROPERTY? GRAPH TYPE (IF NOT EXISTS)? | OR REPLACE PROPERTY? GRAPH TYPE) catalogGraphTypeParentAndName graphTypeSource
--     ;
      def "CreateGraphTypeStatement" $
        record [
          "createOption">: gql "CreateGraphTypeOption",
          "parentAndName">: gql "CatalogGraphTypeParentAndName",
          "source">: gql "GraphTypeSource"],

      def "CreateGraphTypeOption" $
        union [
          "typeIfNotExists">: boolean,
          "orReplace">: unit],

-- graphTypeSource
--     : AS? copyOfGraphType
--     | graphTypeLikeGraph
--     | AS? nestedGraphTypeSpecification
--     ;
      def "GraphTypeSource" $
        union [
          "copyOf">: gql "CopyOfGraphType",
          "likeGraph">: gql "GraphTypeLikeGraph",
          "nestedSpecification">: gql "NestedGraphTypeSpecification"],

-- copyOfGraphType
--     : COPY OF graphTypeReference
--     ;
      def "CopyOfGraphType" $
        gql "GraphTypeReference",

-- // 12.7 <drop graph type statement>
--
-- dropGraphTypeStatement
--     : DROP PROPERTY? GRAPH TYPE (IF EXISTS)? catalogGraphTypeParentAndName
--     ;
      def "DropGraphTypeStatement" $
        record [
          "ifExists">: boolean,
          "parentAndName">: gql "CatalogGraphTypeParentAndName"],

-- // 12.8 <call catalog-modifying statement>
--
-- callCatalogModifyingProcedureStatement
--     : callProcedureStatement
--     ;
      def "CallCatalogModifyingProcedureStatement" $
        gql "CallProcedureStatement",

-- // 13.1 <linear data-modifying statement>
--
-- linearDataModifyingStatement
--     : focusedLinearDataModifyingStatement
--     | ambientLinearDataModifyingStatement
--     ;
      def "LinearDataModifyingStatement" $
        union [
          "focused">: gql "FocusedLinearDataModifyingStatement",
          "ambient">: gql "AmbientLinearDataModifyingStatement"],

-- focusedLinearDataModifyingStatement
--     : focusedLinearDataModifyingStatementBody
--     | focusedNestedDataModifyingProcedureSpecification
--     ;
      def "FocusedLinearDataModifyingStatement" $
        union [
          "simple">: gql "FocusedLinearDataModifyingStatementBody",
          "nested">: gql "FocusedNestedDataModifyingProcedureSpecification"],

-- focusedLinearDataModifyingStatementBody
--     : useGraphClause simpleLinearDataAccessingStatement primitiveResultStatement?
--     ;
      def "FocusedLinearDataModifyingStatementBody" $
        record [
          "useGraph">: gql "UseGraphClause",
          "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
          "primitiveResult">: optional $ gql "PrimitiveResultStatement"],

-- focusedNestedDataModifyingProcedureSpecification
--     : useGraphClause nestedDataModifyingProcedureSpecification
--     ;
      def "FocusedNestedDataModifyingProcedureSpecification" $
        record [
          "useGraph">: gql "UseGraphClause",
          "nestedSpec">: gql "NestedDataModifyingProcedureSpecification"],

-- ambientLinearDataModifyingStatement
--     : ambientLinearDataModifyingStatementBody
--     | nestedDataModifyingProcedureSpecification
--     ;
      def "AmbientLinearDataModifyingStatement" $
        union [
          "simple">: gql "AmbientLinearDataModifyingStatementBody",
          "nested">: gql "NestedDataModifyingProcedureSpecification"],

-- ambientLinearDataModifyingStatementBody
--     : simpleLinearDataAccessingStatement primitiveResultStatement?
--     ;
      def "AmbientLinearDataModifyingStatementBody" $
        record [
          "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
          "primitiveResult">: optional $ gql "PrimitiveResultStatement"],

-- simpleLinearDataAccessingStatement
--     : simpleDataAccessingStatement+
--     ;
      def "SimpleLinearDataAccessingStatement" $
        nonemptyList $ gql "SimpleDataAccessingStatement",

-- simpleDataAccessingStatement
--     : simpleQueryStatement
--     | simpleDataModifyingStatement
--     ;
      def "SimpleDataAccessingStatement" $
        union [
          "query">: gql "SimpleQueryStatement",
          "modifying">: gql "SimpleDataModifyingStatement"],

-- simpleDataModifyingStatement
--     : primitiveDataModifyingStatement
--     | callDataModifyingProcedureStatement
--     ;
      def "SimpleDataModifyingStatement" $
        union [
          "primitive">: gql "PrimitiveDataModifyingStatement",
          "callProcedure">: gql "CallDataModifyingProcedureStatement"],

-- primitiveDataModifyingStatement
--     : insertStatement
--     | setStatement
--     | removeStatement
--     | deleteStatement
--     ;
      def "PrimitiveDataModifyingStatement" $
        union [
          "insert">: gql "InsertStatement",
          "set">: gql "SetStatement",
          "remove">: gql "RemoveStatement",
          "delete">: gql "DeleteStatement"],

-- // 13.2 <insert statement>
--
-- insertStatement
--     : INSERT insertGraphPattern
--     ;
      def "InsertStatement" $
        gql "InsertGraphPattern",

-- // 13.3 <set statement>
--
-- setStatement
--     : SET setItemList
--     ;
      def "SetStatement" $
        gql "SetItemList",

-- setItemList
--     : setItem (COMMA setItem)*
--     ;
      def "SetItemList" $
        nonemptyList $ gql "SetItem",

-- setItem
--     : setPropertyItem
--     | setAllPropertiesItem
--     | setLabelItem
--     ;
      def "SetItem" $
        union [
          "property">: gql "SetPropertyItem",
          "allProperties">: gql "SetAllPropertiesItem",
          "label">: gql "SetLabelItem"],

-- setPropertyItem
--     : bindingVariableReference PERIOD propertyName EQUALS_OPERATOR valueExpression
--     ;
      def "SetPropertyItem" $
        record [
          "variable">: gql "BindingVariableReference",
          "propertyName">: gql "PropertyName",
          "value">: gql "ValueExpression"],

-- setAllPropertiesItem
--     : bindingVariableReference EQUALS_OPERATOR LEFT_BRACE propertyKeyValuePairList? RIGHT_BRACE
--     ;
      def "SetAllPropertiesItem" $
        record [
          "variable">: gql "BindingVariableReference",
          "properties">: optional $ gql "PropertyKeyValuePairList"],

-- setLabelItem
--     : bindingVariableReference isOrColon labelName
--     ;
      def "SetLabelItem" $
        record [
          "variable">: gql "BindingVariableReference",
          "isOrColon">: gql "IsOrColon",
          "label">: gql "LabelName"],

-- // 13.4 <remove statement>
--
-- removeStatement
--     : REMOVE removeItemList
--     ;
      def "RemoveStatement" $
        gql "RemoveItemList",

-- removeItemList
--     : removeItem (COMMA removeItem)*
--     ;
      def "RemoveItemList" $
        nonemptyList $ gql "RemoveItem",

-- removeItem
--     : removePropertyItem
--     | removeLabelItem
--     ;
      def "RemoveItem" $
        union [
          "property">: gql "RemovePropertyItem",
          "label">: gql "RemoveLabelItem"],

-- removePropertyItem
--     : bindingVariableReference PERIOD propertyName
--     ;
      def "RemovePropertyItem" $
        record [
          "variable">: gql "BindingVariableReference",
          "propertyName">: gql "PropertyName"],

-- removeLabelItem
--     : bindingVariableReference isOrColon labelName
--     ;
      def "RemoveLabelItem" $
        record [
          "variable">: gql "BindingVariableReference",
          "isOrColon">: gql "IsOrColon",
          "label">: gql "LabelName"],

-- // 13.5 <delete statement>
--
-- deleteStatement
--     : (DETACH | NODETACH)? DELETE deleteItemList
--     ;
      def "DeleteStatement" $
        record [
          "detach">: optional $ gql "DetachOption",
          "items">: gql "DeleteItemList"],

      def "DetachOption" $
        enum [
          "detach",
          "noDetach"],

-- deleteItemList
--     : deleteItem (COMMA deleteItem)*
--     ;
      def "DeleteItemList" $
        nonemptyList $ gql "DeleteItem",

-- deleteItem
--     : valueExpression
--     ;
      def "DeleteItem" $
        gql "ValueExpression",

-- // 13.6 <call data-modifying procedure statement>
--
-- callDataModifyingProcedureStatement
--     : callProcedureStatement
--     ;
      def "CallDataModifyingProcedureStatement" $
        gql "CallProcedureStatement",

-- // 14.1 <composite query statement>
--
-- compositeQueryStatement
--     : compositeQueryExpression
--     ;
      def "CompositeQueryStatement" $
        gql "CompositeQueryExpression",

-- // 14.2 <composite query expression>
--
-- compositeQueryExpression
--     : compositeQueryExpression queryConjunction compositeQueryPrimary
--     | compositeQueryPrimary
--     ;
      def "CompositeQueryExpression" $
        union [
          "simple">: gql "CompositeQueryExpressionConjunction",
          "primary">: gql "CompositeQueryPrimary"],

      def "CompositeQueryExpressionConjunction" $
        record [
          "left">: gql "CompositeQueryExpression",
          "conjunction">: gql "QueryConjunction",
          "right">: gql "CompositeQueryPrimary"],

-- queryConjunction
--     : setOperator
--     | OTHERWISE
--     ;
      def "QueryConjunction" $
        union [
          "setOperator">: gql "SetOperator",
          "otherwise">: unit],

-- setOperator
--     : UNION setQuantifier?
--     | EXCEPT setQuantifier?
--     | INTERSECT setQuantifier?
--     ;
      def "SetOperator" $
        record [
          "operatorType">: gql "SetOperatorType",
          "quantifier">: optional $ gql "SetQuantifier"],

      def "SetOperatorType" $
        enum ["union", "except", "intersect"],

-- compositeQueryPrimary
--     : linearQueryStatement
--     ;
      def "CompositeQueryPrimary" $
        gql "LinearQueryStatement",

-- // 14.3 <linear query statement> and <simple query statement>
--
-- linearQueryStatement
--     : focusedLinearQueryStatement
--     | ambientLinearQueryStatement
--     ;
      def "LinearQueryStatement" $
        union [
          "focused">: gql "FocusedLinearQueryStatement",
          "ambient">: gql "AmbientLinearQueryStatement"],

-- focusedLinearQueryStatement
--     : focusedLinearQueryStatementPart* focusedLinearQueryAndPrimitiveResultStatementPart
--     | focusedPrimitiveResultStatement
--     | focusedNestedQuerySpecification
--     | selectStatement
--     ;
      def "FocusedLinearQueryStatement" $
        union [
          "parts">: gql "FocusedLinearQueryStatementPartsAndResult",
          "primitive">: gql "FocusedPrimitiveResultStatement",
          "nested">: gql "FocusedNestedQuerySpecification",
          "select">: gql "SelectStatement"],

      def "FocusedLinearQueryStatementPartsAndResult" $
        record [
          "parts">: list $ gql "FocusedLinearQueryStatementPart",
          "result">: gql "FocusedLinearQueryAndPrimitiveResultStatementPart"],

-- focusedLinearQueryStatementPart
--     : useGraphClause simpleLinearQueryStatement
--     ;
      def "FocusedLinearQueryStatementPart" $
        record [
          "useGraph">: gql "UseGraphClause",
          "simple">: gql "SimpleLinearQueryStatement"],

-- focusedLinearQueryAndPrimitiveResultStatementPart
--     : useGraphClause simpleLinearQueryStatement primitiveResultStatement
--     ;
      def "FocusedLinearQueryAndPrimitiveResultStatementPart" $
        record [
          "useGraph">: gql "UseGraphClause",
          "simple">: gql "SimpleLinearQueryStatement",
          "primitiveResult">: gql "PrimitiveResultStatement"],

-- focusedPrimitiveResultStatement
--     : useGraphClause primitiveResultStatement
--     ;
      def "FocusedPrimitiveResultStatement" $
        record [
          "useGraph">: gql "UseGraphClause",
          "primitiveResult">: gql "PrimitiveResultStatement"],

-- focusedNestedQuerySpecification
--     : useGraphClause nestedQuerySpecification
--     ;
      def "FocusedNestedQuerySpecification" $
        record [
          "useGraph">: gql "UseGraphClause",
          "nested">: gql "NestedQuerySpecification"],

-- ambientLinearQueryStatement
--     : simpleLinearQueryStatement? primitiveResultStatement
--     | nestedQuerySpecification
--     ;
      def "AmbientLinearQueryStatement" $
        union [
          "simple">: gql "AmbientLinearQueryStatementSimpleAndPrimitiveResult",
          "nested">: gql "NestedQuerySpecification"],

      def "AmbientLinearQueryStatementSimpleAndPrimitiveResult" $
        record [
          "simple">: optional $ gql "SimpleLinearQueryStatement",
          "primitiveResult">: gql "PrimitiveResultStatement"],

-- simpleLinearQueryStatement
--     : simpleQueryStatement+
--     ;
      def "SimpleLinearQueryStatement" $
        nonemptyList $ gql "SimpleQueryStatement",

-- simpleQueryStatement
--     : primitiveQueryStatement
--     | callQueryStatement
--     ;
      def "SimpleQueryStatement" $
        union [
          "primitive">: gql "PrimitiveQueryStatement",
          "call">: gql "CallQueryStatement"],

-- primitiveQueryStatement
--     : matchStatement
--     | letStatement
--     | forStatement
--     | filterStatement
--     | orderByAndPageStatement
--     ;
      def "PrimitiveQueryStatement" $
        union [
          "match">: gql "MatchStatement",
          "let">: gql "LetStatement",
          "for">: gql "ForStatement",
          "filter">: gql "FilterStatement",
          "orderByAndPage">: gql "OrderByAndPageStatement"],

-- // 14.4 <match statement>
--
-- matchStatement
--     : simpleMatchStatement
--     | optionalMatchStatement
--     ;
      def "MatchStatement" $
        union [
          "simple">: gql "SimpleMatchStatement",
          "optional">: gql "OptionalMatchStatement"],

-- simpleMatchStatement
--     : MATCH graphPatternBindingTable
--     ;
      def "SimpleMatchStatement" $
        gql "GraphPatternBindingTable",

-- optionalMatchStatement
--     : OPTIONAL optionalOperand
--     ;
      def "OptionalMatchStatement" $
        gql "OptionalOperand",

-- optionalOperand
--     : simpleMatchStatement
--     | LEFT_BRACE matchStatementBlock RIGHT_BRACE
--     | LEFT_PAREN matchStatementBlock RIGHT_PAREN
--     ;
      def "OptionalOperand" $
        union [
          "simple">: gql "SimpleMatchStatement",
          "braceBlock">: gql "MatchStatementBlock",
          "parenBlock">: gql "MatchStatementBlock"],

-- matchStatementBlock
--     : matchStatement+
--     ;
      def "MatchStatementBlock" $
        nonemptyList $ gql "MatchStatement",

-- // 14.5 <call query statement>
--
-- callQueryStatement
--     : callProcedureStatement
--     ;
      def "CallQueryStatement" $
        gql "CallProcedureStatement",

-- // 14.6 <filter statement>
--
-- filterStatement
--     : FILTER (whereClause | searchCondition)
--     ;
      def "FilterStatement" $
        union [
          "whereClause">: gql "WhereClause",
          "searchCondition">: gql "SearchCondition"],

-- // 14.7 <let statement>
--
-- letStatement
--     : LET letVariableDefinitionList
--     ;
      def "LetStatement" $
        gql "LetVariableDefinitionList",

-- letVariableDefinitionList
--     : letVariableDefinition (COMMA letVariableDefinition)*
--     ;
      def "LetVariableDefinitionList" $
        nonemptyList $ gql "LetVariableDefinition",

-- letVariableDefinition
--     : valueVariableDefinition
--     | bindingVariable EQUALS_OPERATOR valueExpression
--     ;
      def "LetVariableDefinition" $
        union [
          "valueVariable">: gql "ValueVariableDefinition",
          "bindingEqualsValue">: gql "BindingEqualsValue"],

      def "BindingEqualsValue" $
        record [
          "binding">: gql "BindingVariable",
          "value">: gql "ValueExpression"],

-- // 14.8 <for statement>
--
-- forStatement
--     : FOR forItem forOrdinalityOrOffset?
--     ;
      def "ForStatement" $
        record [
          "item">: gql "ForItem",
          "ordinalityOrOffset">: optional $ gql "ForOrdinalityOrOffset"],

-- forItem
--     : forItemAlias forItemSource
--     ;
      def "ForItem" $
        record [
          "alias">: gql "ForItemAlias",
          "source">: gql "ForItemSource"],

-- forItemAlias
--     : bindingVariable IN
--     ;
      def "ForItemAlias" $
        gql "BindingVariable",

-- forItemSource
--     : valueExpression
--     ;
      def "ForItemSource" $
        gql "ValueExpression",

-- forOrdinalityOrOffset
--     : WITH (ORDINALITY | OFFSET) bindingVariable
--     ;
      def "ForOrdinalityOrOffset" $
        record [
          "type">: gql "OrdinalityOrOffsetType",
          "variable">: gql "BindingVariable"],

      def "OrdinalityOrOffsetType" $
        enum ["ordinality", "offset"],

-- // 14.9 <order by and page statement>
--
-- orderByAndPageStatement
--     : orderByClause offsetClause? limitClause?
--     | offsetClause limitClause?
--     | limitClause
--     ;
      def "OrderByAndPageStatement" $
        union [
          "orderByAndOptionalOffsetAndLimit">: gql "OrderByAndOptionalOffsetAndLimit",
          "offsetAndOptionalLimit">: gql "OffsetAndOptionalLimit",
          "limitOnly">: gql "LimitClause"],

      def "OrderByAndOptionalOffsetAndLimit" $
        record [
          "orderBy">: gql "OrderByClause",
          "offset">: optional $ gql "OffsetClause",
          "limit">: optional $ gql "LimitClause"],

      def "OffsetAndOptionalLimit" $
        record [
          "offset">: gql "OffsetClause",
          "limit">: optional $ gql "LimitClause"],

-- // 14.10 <primitive result statement>
--
-- primitiveResultStatement
--     : returnStatement orderByAndPageStatement?
--     | FINISH
--     ;
      def "PrimitiveResultStatement" $
        union [
          "returnAndOptionalOrderBy">: gql "ReturnAndOptionalOrderByAndPage",
          "finish">: unit],

      def "ReturnAndOptionalOrderByAndPage" $
        record [
          "return">: gql "ReturnStatement",
          "orderByAndPage">: optional $ gql "OrderByAndPageStatement"],

-- // 14.11 <return statement>
--
-- returnStatement
--     : RETURN returnStatementBody
--     ;
      def "ReturnStatement" $
        gql "ReturnStatementBody",

-- returnStatementBody
--     : setQuantifier? (ASTERISK | returnItemList) groupByClause?
--     | NO BINDINGS
--     ;
      def "ReturnStatementBody" $
        union [
          "items">: gql "ReturnItemsAndGroupBy",
          "noBindings">: unit],

      def "ReturnItemsAndGroupBy" $
        record [
          "quantifier">: optional $ gql "SetQuantifier",
          "items">: gql "ReturnItems",
          "groupBy">: optional $ gql "GroupByClause"],

-- returnItems
--     : ASTERISK
--     | returnItemList
--     ;
      def "ReturnItems" $
        union [
          "asterisk">: unit,
          "itemList">: gql "ReturnItemList"],

-- returnItemList
--     : returnItem (COMMA returnItem)*
--     ;
      def "ReturnItemList" $
        nonemptyList $ gql "ReturnItem",

-- returnItem
--     : aggregatingValueExpression returnItemAlias?
--     ;
      def "ReturnItem" $
        record [
          "expression">: gql "AggregatingValueExpression",
          "alias">: optional $ gql "ReturnItemAlias"],

-- returnItemAlias
--     : AS identifier
--     ;
      def "ReturnItemAlias" $
        string,

-- // 14.12 <select statement>
--
-- selectStatement
--     : SELECT setQuantifier? (ASTERISK | selectItemList) (selectStatementBody whereClause? groupByClause? havingClause? orderByClause? offsetClause? limitClause?)?
--     ;
      def "SelectStatement" $
        record [
          "quantifier">: optional $ gql "SetQuantifier",
          "items">: gql "SelectItems",
          "body">: optional $ gql "SelectStatementBodyAndClauses"],

      def "SelectItems" $
        union [
          "asterisk">: unit,
          "itemList">: gql "SelectItemList"],

      def "SelectStatementBodyAndClauses" $
        record [
          "body">: gql "SelectStatementBody",
          "where">: optional $ gql "WhereClause",
          "groupBy">: optional $ gql "GroupByClause",
          "having">: optional $ gql "HavingClause",
          "orderBy">: optional $ gql "OrderByClause",
          "offset">: optional $ gql "OffsetClause",
          "limit">: optional $ gql "LimitClause"],

-- selectItemList
--     : selectItem (COMMA selectItem)*
--     ;
      def "SelectItemList" $
        nonemptyList $ gql "SelectItem",

-- selectItem
--     : aggregatingValueExpression selectItemAlias?
--     ;
      def "SelectItem" $
        record [
          "expression">: gql "AggregatingValueExpression",
          "alias">: optional $ gql "SelectItemAlias"],

-- selectItemAlias
--     : AS identifier
--     ;
      def "SelectItemAlias" $
        string,

-- havingClause
--     : HAVING searchCondition
--     ;
      def "HavingClause" $
        gql "SearchCondition",

-- selectStatementBody
--     : FROM (selectGraphMatchList | selectQuerySpecification)
--     ;
      def "SelectStatementBody" $
        union [
          "graphMatchList">: gql "SelectGraphMatchList",
          "querySpecification">: gql "SelectQuerySpecification"],

-- selectGraphMatchList
--     : selectGraphMatch (COMMA selectGraphMatch)*
--     ;
      def "SelectGraphMatchList" $
        nonemptyList $ gql "SelectGraphMatch",

-- selectGraphMatch
--     : graphExpression matchStatement
--     ;
      def "SelectGraphMatch" $
        record [
          "graphExpression">: gql "GraphExpression",
          "matchStatement">: gql "MatchStatement"],

-- selectQuerySpecification
--     : nestedQuerySpecification
--     | graphExpression nestedQuerySpecification
--     ;
      def "SelectQuerySpecification" $
        union [
          "nested">: gql "NestedQuerySpecification",
          "graphAndNested">: gql "GraphAndNestedQuerySpecification"],

      def "GraphAndNestedQuerySpecification" $
        record [
          "graphExpression">: gql "GraphExpression",
          "nested">: gql "NestedQuerySpecification"],

-- // 15.1 <call procedure statement> and <procedure call>
--
-- callProcedureStatement
--     : OPTIONAL? CALL procedureCall
--     ;
      def "CallProcedureStatement" $
        record [
          "optional">: boolean,
          "call">: gql "ProcedureCall"],

-- procedureCall
--     : inlineProcedureCall
--     | namedProcedureCall
--     ;
      def "ProcedureCall" $
        union [
          "inline">: gql "InlineProcedureCall",
          "named">: gql "NamedProcedureCall"],

-- // 15.2 <inline procedure call>
--
-- inlineProcedureCall
--     : variableScopeClause? nestedProcedureSpecification
--     ;
      def "InlineProcedureCall" $
        record [
          "scope">: optional $ gql "VariableScopeClause",
          "nested">: gql "NestedProcedureSpecification"],

-- variableScopeClause
--     : LEFT_PAREN bindingVariableReferenceList? RIGHT_PAREN
--     ;
      def "VariableScopeClause" $
        optional $ gql "BindingVariableReferenceList",

-- bindingVariableReferenceList
--     : bindingVariableReference (COMMA bindingVariableReference)*
--     ;
      def "BindingVariableReferenceList" $
        nonemptyList $ gql "BindingVariableReference",

-- // 15.3 <named procedure call>
--
-- namedProcedureCall
--     : procedureReference LEFT_PAREN procedureArgumentList? RIGHT_PAREN yieldClause?
--     ;
      def "NamedProcedureCall" $
        record [
          "reference">: gql "ProcedureReference",
          "arguments">: optional $ gql "ProcedureArgumentList",
          "yield">: optional $ gql "YieldClause"],

-- procedureArgumentList
--     : procedureArgument (COMMA procedureArgument)*
--     ;
      def "ProcedureArgumentList" $
        nonemptyList $ gql "ProcedureArgument",

-- procedureArgument
--     : valueExpression
--     ;
      def "ProcedureArgument" $
        gql "ValueExpression",

-- // 16.1 <at schema clause>
--
-- atSchemaClause
--     : AT schemaReference
--     ;
      def "AtSchemaClause" $
        gql "SchemaReference",

-- // 16.2 <use graph clause>
--
-- useGraphClause
--     : USE graphExpression
--     ;
      def "UseGraphClause" $
        gql "GraphExpression",

-- // 16.3 <graph pattern binding table>
--
-- graphPatternBindingTable
--     : graphPattern graphPatternYieldClause?
--     ;
      def "GraphPatternBindingTable" $
        record [
          "pattern">: gql "GraphPattern",
          "yieldClause">: optional $ gql "GraphPatternYieldClause"],

-- graphPatternYieldClause
--     : YIELD graphPatternYieldItemList
--     ;
      def "GraphPatternYieldClause" $
        gql "GraphPatternYieldItemList",

-- graphPatternYieldItemList
--     : graphPatternYieldItem (COMMA graphPatternYieldItem)*
--     | NO BINDINGS
--     ;
      def "GraphPatternYieldItemList" $
        union [
          "items">: nonemptyList $ gql "GraphPatternYieldItem",
          "noBindings">: unit],

-- graphPatternYieldItem
--     : bindingVariableReference
-- //    : elementVariableReference
-- //    | pathVariableReference
--     ;
      def "GraphPatternYieldItem" $
        gql "BindingVariableReference",

-- // 16.4 <graph pattern>
--
-- graphPattern
--     : matchMode? pathPatternList keepClause? graphPatternWhereClause?
--     ;
      def "GraphPattern" $
        record [
          "matchMode">: optional $ gql "MatchMode",
          "pathPatterns">: gql "PathPatternList",
          "keepClause">: optional $ gql "KeepClause",
          "whereClause">: optional $ gql "GraphPatternWhereClause"],

-- matchMode
--     : repeatableElementsMatchMode
--     | differentEdgesMatchMode
--     ;
      def "MatchMode" $
        union [
          "repeatableElements">: gql "RepeatableElementsMatchMode",
          "differentEdges">: gql "DifferentEdgesMatchMode"],

-- repeatableElementsMatchMode
--     : REPEATABLE elementBindingsOrElements
--     ;
      def "RepeatableElementsMatchMode" $
        gql "ElementBindingsOrElements",

-- differentEdgesMatchMode
--     : DIFFERENT edgeBindingsOrEdges
--     ;
      def "DifferentEdgesMatchMode" $
        gql "EdgeBindingsOrEdges",

-- elementBindingsOrElements
--     : ELEMENT BINDINGS?
--     | ELEMENTS
--     ;
      def "ElementBindingsOrElements" $
        union [
          "elementBindings">: boolean,
          "elements">: unit],

-- edgeBindingsOrEdges
--     : edgeSynonym BINDINGS?
--     | edgesSynonym
--     ;
      def "EdgeBindingsOrEdges" $
        union [
          "edgeBindings">: boolean,
          "edges">: unit],

-- pathPatternList
--     : pathPattern (COMMA pathPattern)*
--     ;
      def "PathPatternList" $
        nonemptyList $ gql "PathPattern",

-- pathPattern
--     : pathVariableDeclaration? pathPatternPrefix? pathPatternExpression
--     ;
      def "PathPattern" $
        record [
          "variableDeclaration">: optional $ gql "PathVariableDeclaration",
          "prefix">: optional $ gql "PathPatternPrefix",
          "expression">: gql "PathPatternExpression"],

-- pathVariableDeclaration
--     : pathVariable EQUALS_OPERATOR
--     ;
      def "PathVariableDeclaration" $
        gql "PathVariable",

-- keepClause
--     : KEEP pathPatternPrefix
--     ;
      def "KeepClause" $
        gql "PathPatternPrefix",

-- graphPatternWhereClause
--     : WHERE searchCondition
--     ;
      def "GraphPatternWhereClause" $
        gql "SearchCondition",

-- // 16.5 <insert graph pattern>
--
-- insertGraphPattern
--     : insertPathPatternList
--     ;
      def "InsertGraphPattern" $
        gql "InsertPathPatternList",

-- insertPathPatternList
--     : insertPathPattern (COMMA insertPathPattern)*
--     ;
      def "InsertPathPatternList" $
        nonemptyList $ gql "InsertPathPattern",

-- insertPathPattern
--     : insertNodePattern (insertEdgePattern insertNodePattern)*
--     ;
      def "InsertPathPattern" $
        record [
          "startNode">: gql "InsertNodePattern",
          "edgesAndNodes">: list $ gql "InsertEdgeAndNode"],

      def "InsertEdgeAndNode" $
        record [
          "edge">: gql "InsertEdgePattern",
          "node">: gql "InsertNodePattern"],

-- insertNodePattern
--     : LEFT_PAREN insertElementPatternFiller? RIGHT_PAREN
--     ;
      def "InsertNodePattern" $
        optional $ gql "InsertElementPatternFiller",

-- insertEdgePattern
--     : insertEdgePointingLeft
--     | insertEdgePointingRight
--     | insertEdgeUndirected
--     ;
      def "InsertEdgePattern" $
        union [
          "pointingLeft">: gql "InsertEdgePointingLeft",
          "pointingRight">: gql "InsertEdgePointingRight",
          "undirected">: gql "InsertEdgeUndirected"],

-- insertEdgePointingLeft
--     : LEFT_ARROW_BRACKET insertElementPatternFiller? RIGHT_BRACKET_MINUS
--     ;
      def "InsertEdgePointingLeft" $
        optional $ gql "InsertElementPatternFiller",

-- insertEdgePointingRight
--     : MINUS_LEFT_BRACKET insertElementPatternFiller? BRACKET_RIGHT_ARROW
--     ;
      def "InsertEdgePointingRight" $
        optional $ gql "InsertElementPatternFiller",

-- insertEdgeUndirected
--     : TILDE_LEFT_BRACKET insertElementPatternFiller? RIGHT_BRACKET_TILDE
--     ;
      def "InsertEdgeUndirected" $
        optional $ gql "InsertElementPatternFiller",

-- insertElementPatternFiller
--     : elementVariableDeclaration labelAndPropertySetSpecification?
--     | elementVariableDeclaration? labelAndPropertySetSpecification
--     ;
      def "InsertElementPatternFiller" $
        record [
          "variableDeclaration">: optional $ gql "ElementVariableDeclaration",
          "labelAndProperties">: optional $ gql "LabelAndPropertySetSpecification"],

-- labelAndPropertySetSpecification
--     : isOrColon labelSetSpecification elementPropertySpecification?
--     | (isOrColon labelSetSpecification)? elementPropertySpecification
--     ;
      def "LabelAndPropertySetSpecification" $
        record [
          "isOrColon">: optional $ gql "IsOrColon",
          "labelSet">: optional $ gql "LabelSetSpecification",
          "propertySpecification">: optional $ gql "ElementPropertySpecification"],

-- // 16.6 <path pattern prefix>
--
-- pathPatternPrefix
--     : pathModePrefix
--     | pathSearchPrefix
--     ;
      def "PathPatternPrefix" $
        union [
          "modePrefix">: gql "PathModePrefix",
          "searchPrefix">: gql "PathSearchPrefix"],

-- pathModePrefix
--     : pathMode pathOrPaths?
--     ;
      def "PathModePrefix" $
        record [
          "mode">: gql "PathMode",
          "orPaths">: optional $ gql "PathOrPaths"],

-- pathMode
--     : WALK
--     | TRAIL
--     | SIMPLE
--     | ACYCLIC
--     ;
      def "PathMode" $
        enum ["walk", "trail", "simple", "acyclic"],

-- pathSearchPrefix
--     : allPathSearch
--     | anyPathSearch
--     | shortestPathSearch
--     ;
      def "PathSearchPrefix" $
        union [
          "all">: gql "AllPathSearch",
          "any">: gql "AnyPathSearch",
          "shortest">: gql "ShortestPathSearch"],

-- allPathSearch
--     : ALL pathMode? pathOrPaths?
--     ;
      def "AllPathSearch" $
        record [
          "mode">: optional $ gql "PathMode",
          "orPaths">: optional $ gql "PathOrPaths"],

-- pathOrPaths
--     : PATH
--     | PATHS
--     ;
      def "PathOrPaths" $
        enum ["path", "paths"],

-- anyPathSearch
--     : ANY numberOfPaths? pathMode? pathOrPaths?
--     ;
      def "AnyPathSearch" $
        record [
          "numberOfPaths">: optional $ gql "NumberOfPaths",
          "mode">: optional $ gql "PathMode",
          "orPaths">: optional $ gql "PathOrPaths"],

-- numberOfPaths
--     : nonNegativeIntegerSpecification
--     ;
      def "NumberOfPaths" $
        gql "NonNegativeIntegerSpecification",

-- shortestPathSearch
--     : allShortestPathSearch
--     | anyShortestPathSearch
--     | countedShortestPathSearch
--     | countedShortestGroupSearch
--     ;
      def "ShortestPathSearch" $
        union [
          "allShortest">: gql "AllShortestPathSearch",
          "anyShortest">: gql "AnyShortestPathSearch",
          "countedShortest">: gql "CountedShortestPathSearch",
          "countedShortestGroup">: gql "CountedShortestGroupSearch"],

-- allShortestPathSearch
--     : ALL SHORTEST pathMode? pathOrPaths?
--     ;
      def "AllShortestPathSearch" $
        record [
          "mode">: optional $ gql "PathMode",
          "orPaths">: optional $ gql "PathOrPaths"],

-- anyShortestPathSearch
--     : ANY SHORTEST pathMode? pathOrPaths?
--     ;
      def "AnyShortestPathSearch" $
        record [
          "mode">: optional $ gql "PathMode",
          "orPaths">: optional $ gql "PathOrPaths"],

-- countedShortestPathSearch
--     : SHORTEST numberOfPaths pathMode? pathOrPaths?
--     ;
      def "CountedShortestPathSearch" $
        record [
          "numberOfPaths">: gql "NumberOfPaths",
          "mode">: optional $ gql "PathMode",
          "orPaths">: optional $ gql "PathOrPaths"],

-- countedShortestGroupSearch
--     : SHORTEST numberOfGroups? pathMode? pathOrPaths? (GROUP | GROUPS)
--     ;
      def "CountedShortestGroupSearch" $
        record [
          "numberOfGroups">: optional $ gql "NumberOfGroups",
          "mode">: optional $ gql "PathMode",
          "orPaths">: optional $ gql "PathOrPaths",
          "groups">: boolean],

-- numberOfGroups
--     : nonNegativeIntegerSpecification
--     ;
      def "NumberOfGroups" $
        gql "NonNegativeIntegerSpecification",

-- // 16.7 <path pattern expression>
--
-- pathPatternExpression
--     : pathTerm                                              #ppePathTerm
--     | pathTerm (MULTISET_ALTERNATION_OPERATOR pathTerm)+    #ppeMultisetAlternation
--     | pathTerm (VERTICAL_BAR pathTerm)+                     #ppePatternUnion
--     ;
      def "PathPatternExpression" $
        union [
          "term">: gql "PathTerm",
          "multisetAlternation">: nonemptyList $ gql "PathTerm",
          "patternUnion">: nonemptyList $ gql "PathTerm"],

-- pathTerm
--     : pathFactor+
--     ;
      def "PathTerm" $
        nonemptyList $ gql "PathFactor",

-- pathFactor
--     : pathPrimary                           #pfPathPrimary
--     | pathPrimary graphPatternQuantifier    #pfQuantifiedPathPrimary
--     | pathPrimary QUESTION_MARK             #pfQuestionedPathPrimary
--     ;
      def "PathFactor" $
        union [
          "primary">: gql "PathPrimary",
          "quantifiedPrimary">: gql "QuantifiedPathPrimary",
          "questionedPrimary">: gql "QuestionedPathPrimary"],

      def "QuantifiedPathPrimary" $
        record [
          "primary">: gql "PathPrimary",
          "quantifier">: gql "GraphPatternQuantifier"],

      def "QuestionedPathPrimary" $
        gql "PathPrimary",

-- pathPrimary
--     : elementPattern                        #ppElementPattern
--     | parenthesizedPathPatternExpression    #ppParenthesizedPathPatternExpression
--     | simplifiedPathPatternExpression       #ppSimplifiedPathPatternExpression
--     ;
      def "PathPrimary" $
        union [
          "elementPattern">: gql "ElementPattern",
          "parenthesizedExpression">: gql "ParenthesizedPathPatternExpression",
          "simplifiedExpression">: gql "SimplifiedPathPatternExpression"],

-- elementPattern
--     : nodePattern
--     | edgePattern
--     ;
      def "ElementPattern" $
        union [
          "node">: gql "NodePattern",
          "edge">: gql "EdgePattern"],

-- nodePattern
--     : LEFT_PAREN elementPatternFiller RIGHT_PAREN
--     ;
      def "NodePattern" $
        gql "ElementPatternFiller",

-- elementPatternFiller
--     : elementVariableDeclaration? isLabelExpression? elementPatternPredicate?
--     ;
      def "ElementPatternFiller" $
        record [
          "variableDeclaration">: optional $ gql "ElementVariableDeclaration",
          "isLabelExpression">: optional $ gql "IsLabelExpression",
          "predicate">: optional $ gql "ElementPatternPredicate"],

-- elementVariableDeclaration
--     : TEMP? elementVariable
--     ;
      def "ElementVariableDeclaration" $
        record [
          "temp">: optional boolean,
          "variable">: gql "ElementVariable"],

-- isLabelExpression
--     : isOrColon labelExpression
--     ;
      def "IsLabelExpression" $
        record [
          "isOrColon">: gql "IsOrColon",
          "label">: gql "LabelExpression"],

-- isOrColon
--     : IS
--     | COLON
--     ;
      def "IsOrColon" $
        enum ["is", "colon"],

-- elementPatternPredicate
--     : elementPatternWhereClause
--     | elementPropertySpecification
--     ;
      def "ElementPatternPredicate" $
        union [
          "whereClause">: gql "ElementPatternWhereClause",
          "propertySpecification">: gql "ElementPropertySpecification"],

-- elementPatternWhereClause
--     : WHERE searchCondition
--     ;
      def "ElementPatternWhereClause" $
        gql "SearchCondition",

-- elementPropertySpecification
--     : LEFT_BRACE propertyKeyValuePairList RIGHT_BRACE
--     ;
      def "ElementPropertySpecification" $
        gql "PropertyKeyValuePairList",

-- propertyKeyValuePairList
--     : propertyKeyValuePair (COMMA propertyKeyValuePair)*
--     ;
      def "PropertyKeyValuePairList" $
        nonemptyList $ gql "PropertyKeyValuePair",

-- propertyKeyValuePair
--     : propertyName COLON valueExpression
--     ;
      def "PropertyKeyValuePair" $
        record [
          "name">: gql "PropertyName",
          "value">: gql "ValueExpression"],

-- edgePattern
--     : fullEdgePattern
--     | abbreviatedEdgePattern
--     ;
      def "EdgePattern" $
        union [
          "fullEdge">: gql "FullEdgePattern",
          "abbreviatedEdge">: gql "AbbreviatedEdgePattern"],

-- fullEdgePattern
--     : fullEdgePointingLeft
--     | fullEdgeUndirected
--     | fullEdgePointingRight
--     | fullEdgeLeftOrUndirected
--     | fullEdgeUndirectedOrRight
--     | fullEdgeLeftOrRight
--     | fullEdgeAnyDirection
--     ;
      def "FullEdgePattern" $
        union [
          "pointingLeft">: gql "FullEdgePointingLeft",
          "undirected">: gql "FullEdgeUndirected",
          "pointingRight">: gql "FullEdgePointingRight",
          "leftOrUndirected">: gql "FullEdgeLeftOrUndirected",
          "undirectedOrRight">: gql "FullEdgeUndirectedOrRight",
          "leftOrRight">: gql "FullEdgeLeftOrRight",
          "anyDirection">: gql "FullEdgeAnyDirection"],

-- fullEdgePointingLeft
--     : LEFT_ARROW_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS
--     ;
      def "FullEdgePointingLeft" $
        gql "ElementPatternFiller",

-- fullEdgeUndirected
--     : TILDE_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE
--     ;
      def "FullEdgeUndirected" $
        gql "ElementPatternFiller",

-- fullEdgePointingRight
--     : MINUS_LEFT_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW
--     ;
      def "FullEdgePointingRight" $
        gql "ElementPatternFiller",

-- fullEdgeLeftOrUndirected
--     : LEFT_ARROW_TILDE_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE
--     ;
      def "FullEdgeLeftOrUndirected" $
        gql "ElementPatternFiller",

-- fullEdgeUndirectedOrRight
--     : TILDE_LEFT_BRACKET elementPatternFiller BRACKET_TILDE_RIGHT_ARROW
--     ;
      def "FullEdgeUndirectedOrRight" $
        gql "ElementPatternFiller",

-- fullEdgeLeftOrRight
--     : LEFT_ARROW_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW
--     ;
      def "FullEdgeLeftOrRight" $
        gql "ElementPatternFiller",

-- fullEdgeAnyDirection
--     : MINUS_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS
--     ;
      def "FullEdgeAnyDirection" $
        gql "ElementPatternFiller",

-- abbreviatedEdgePattern
--     : LEFT_ARROW
--     | TILDE
--     | RIGHT_ARROW
--     | LEFT_ARROW_TILDE
--     | TILDE_RIGHT_ARROW
--     | LEFT_MINUS_RIGHT
--     | MINUS_SIGN
--     ;
      def "AbbreviatedEdgePattern" $
        enum ["leftArrow", "tilde", "rightArrow", "leftArrowTilde", "tildeRightArrow", "leftMinusRight", "minusSign"],

-- parenthesizedPathPatternExpression
--     : LEFT_PAREN subpathVariableDeclaration? pathModePrefix? pathPatternExpression parenthesizedPathPatternWhereClause? RIGHT_PAREN
--     ;
      def "ParenthesizedPathPatternExpression" $
        record [
          "subpathDeclaration">: optional $ gql "SubpathVariableDeclaration",
          "pathMode">: optional $ gql "PathModePrefix",
          "expression">: gql "PathPatternExpression",
          "whereClause">: optional $ gql "ParenthesizedPathPatternWhereClause"],

-- subpathVariableDeclaration
--     : subpathVariable EQUALS_OPERATOR
--     ;
      def "SubpathVariableDeclaration" $
        gql "SubpathVariable",

-- parenthesizedPathPatternWhereClause
--     : WHERE searchCondition
--     ;
      def "ParenthesizedPathPatternWhereClause" $
        gql "SearchCondition",

-- // 16.8 <label expression>
--
-- labelExpression
--     : EXCLAMATION_MARK labelExpression                  #labelExpressionNegation
--     | labelExpression AMPERSAND labelExpression         #labelExpressionConjunction
--     | labelExpression VERTICAL_BAR labelExpression      #labelExpressionDisjunction
--     | labelName                                         #labelExpressionName
--     | PERCENT                                           #labelExpressionWildcard
--     | LEFT_PAREN labelExpression RIGHT_PAREN            #labelExpressionParenthesized
--     ;
      def "LabelExpression" $
        union [
          "negation">: gql "LabelExpression",
          "conjunction">: gql "ConjunctionLabelExpression",
          "disjunction">: gql "DisjunctionLabelExpression",
          "name">: gql "LabelName",
          "wildcard">: unit,
          "parenthesized">: gql "LabelExpression"],

      def "ConjunctionLabelExpression" $
        record [
          "left">: gql "LabelExpression",
          "right">: gql "LabelExpression"],

      def "DisjunctionLabelExpression" $
        record [
          "left">: gql "LabelExpression",
          "right">: gql "LabelExpression"],

-- // 16.9 <path variable reference>
--
-- pathVariableReference
--     : bindingVariableReference
--     ;
      def "PathVariableReference" $
        gql "BindingVariableReference",

-- // 16.10 <element variable reference>
--
-- elementVariableReference
--     : bindingVariableReference
--     ;
      def "ElementVariableReference" $
        gql "BindingVariableReference",

-- // 16.11 <graph pattern quantifier>
--
-- graphPatternQuantifier
--     : ASTERISK
--     | PLUS_SIGN
--     | fixedQuantifier
--     | generalQuantifier
--     ;
      def "GraphPatternQuantifier" $
        union [
          "asterisk">: unit,
          "plusSign">: unit,
          "fixed">: gql "FixedQuantifier",
          "general">: gql "GeneralQuantifier"],

-- fixedQuantifier
--     : LEFT_BRACE unsignedInteger RIGHT_BRACE
--     ;
      def "FixedQuantifier" $
        gql "UnsignedInteger",

-- generalQuantifier
--     : LEFT_BRACE lowerBound? COMMA upperBound? RIGHT_BRACE
--     ;
      def "GeneralQuantifier" $
        record [
          "lowerBound">: optional $ gql "LowerBound",
          "upperBound">: optional $ gql "UpperBound"],

-- lowerBound
--     : unsignedInteger
--     ;
      def "LowerBound" $
        gql "UnsignedInteger",

-- upperBound
--     : unsignedInteger
--     ;
      def "UpperBound" $
        gql "UnsignedInteger",

-- // 16.12 <simplified path pattern expression>
--
-- simplifiedPathPatternExpression
--     : simplifiedDefaultingLeft
--     | simplifiedDefaultingUndirected
--     | simplifiedDefaultingRight
--     | simplifiedDefaultingLeftOrUndirected
--     | simplifiedDefaultingUndirectedOrRight
--     | simplifiedDefaultingLeftOrRight
--     | simplifiedDefaultingAnyDirection
--     ;
      def "SimplifiedPathPatternExpression" $
        union [
          "left">: gql "SimplifiedDefaultingLeft",
          "undirected">: gql "SimplifiedDefaultingUndirected",
          "right">: gql "SimplifiedDefaultingRight",
          "leftOrUndirected">: gql "SimplifiedDefaultingLeftOrUndirected",
          "undirectedOrRight">: gql "SimplifiedDefaultingUndirectedOrRight",
          "leftOrRight">: gql "SimplifiedDefaultingLeftOrRight",
          "anyDirection">: gql "SimplifiedDefaultingAnyDirection"],

-- simplifiedDefaultingLeft
--     : LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS
--     ;
      def "SimplifiedDefaultingLeft" $
        gql "SimplifiedContents",

-- simplifiedDefaultingUndirected
--     : TILDE_SLASH simplifiedContents SLASH_TILDE
--     ;
      def "SimplifiedDefaultingUndirected" $
        gql "SimplifiedContents",

-- simplifiedDefaultingRight
--     : MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT
--     ;
      def "SimplifiedDefaultingRight" $
        gql "SimplifiedContents",

-- simplifiedDefaultingLeftOrUndirected
--     : LEFT_TILDE_SLASH simplifiedContents SLASH_TILDE
--     ;
      def "SimplifiedDefaultingLeftOrUndirected" $
        gql "SimplifiedContents",

-- simplifiedDefaultingUndirectedOrRight
--     : TILDE_SLASH simplifiedContents SLASH_TILDE_RIGHT
--     ;
      def "SimplifiedDefaultingUndirectedOrRight" $
        gql "SimplifiedContents",

-- simplifiedDefaultingLeftOrRight
--     : LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT
--     ;
      def "SimplifiedDefaultingLeftOrRight" $
        gql "SimplifiedContents",

-- simplifiedDefaultingAnyDirection
--     : MINUS_SLASH simplifiedContents SLASH_MINUS
--     ;
      def "SimplifiedDefaultingAnyDirection" $
        gql "SimplifiedContents",

-- simplifiedContents
--     : simplifiedTerm
--     | simplifiedPathUnion
--     | simplifiedMultisetAlternation
--     ;
      def "SimplifiedContents" $
        union [
          "term">: gql "SimplifiedTerm",
          "pathUnion">: gql "SimplifiedPathUnion",
          "multisetAlternation">: gql "SimplifiedMultisetAlternation"],

-- simplifiedPathUnion
--     : simplifiedTerm VERTICAL_BAR simplifiedTerm (VERTICAL_BAR simplifiedTerm)*
--     ;
      def "SimplifiedPathUnion" $
        nonemptyList $ gql "SimplifiedTerm",

-- simplifiedMultisetAlternation
--     : simplifiedTerm MULTISET_ALTERNATION_OPERATOR simplifiedTerm (MULTISET_ALTERNATION_OPERATOR simplifiedTerm)*
--     ;
      def "SimplifiedMultisetAlternation" $
        nonemptyList $ gql "SimplifiedTerm",

-- simplifiedTerm
--     : simplifiedFactorLow                        #simplifiedFactorLowLabel
--     | simplifiedTerm simplifiedFactorLow      #simplifiedConcatenationLabel
--     ;
      def "SimplifiedTerm" $
        union [
          "factorLow">: gql "SimplifiedFactorLow",
          "concatenation">: gql "SimplifiedConcatenation"],

      def "SimplifiedConcatenation" $
        record [
          "initialTerm">: gql "SimplifiedTerm",
          "nextFactor">: gql "SimplifiedFactorLow"],

-- simplifiedFactorLow
--     : simplifiedFactorHigh                                     #simplifiedFactorHighLabel
--     | simplifiedFactorLow AMPERSAND simplifiedFactorHigh #simplifiedConjunctionLabel
--     ;
      def "SimplifiedFactorLow" $
        union [
          "factorHigh">: gql "SimplifiedFactorHigh",
          "conjunction">: gql "SimplifiedConjunction"],

      def "SimplifiedConjunction" $
        record [
          "left">: gql "SimplifiedFactorLow",
          "right">: gql "SimplifiedFactorHigh"],

-- simplifiedFactorHigh
--     : simplifiedTertiary
--     | simplifiedQuantified
--     | simplifiedQuestioned
--     ;
      def "SimplifiedFactorHigh" $
        union [
          "tertiary">: gql "SimplifiedTertiary",
          "quantified">: gql "SimplifiedQuantified",
          "questioned">: gql "SimplifiedQuestioned"],

-- simplifiedQuantified
--     : simplifiedTertiary graphPatternQuantifier
--     ;
      def "SimplifiedQuantified" $
        record [
          "tertiary">: gql "SimplifiedTertiary",
          "quantifier">: gql "GraphPatternQuantifier"],

-- simplifiedQuestioned
--     : simplifiedTertiary QUESTION_MARK
--     ;
      def "SimplifiedQuestioned" $
        gql "SimplifiedTertiary",

-- simplifiedTertiary
--     : simplifiedDirectionOverride
--     | simplifiedSecondary
--     ;
      def "SimplifiedTertiary" $
        union [
          "directionOverride">: gql "SimplifiedDirectionOverride",
          "secondary">: gql "SimplifiedSecondary"],

-- simplifiedDirectionOverride
--     : simplifiedOverrideLeft
--     | simplifiedOverrideUndirected
--     | simplifiedOverrideRight
--     | simplifiedOverrideLeftOrUndirected
--     | simplifiedOverrideUndirectedOrRight
--     | simplifiedOverrideLeftOrRight
--     | simplifiedOverrideAnyDirection
--     ;
      def "SimplifiedDirectionOverride" $
        union [
          "overrideLeft">: gql "SimplifiedOverrideLeft",
          "overrideUndirected">: gql "SimplifiedOverrideUndirected",
          "overrideRight">: gql "SimplifiedOverrideRight",
          "overrideLeftOrUndirected">: gql "SimplifiedOverrideLeftOrUndirected",
          "overrideUndirectedOrRight">: gql "SimplifiedOverrideUndirectedOrRight",
          "overrideLeftOrRight">: gql "SimplifiedOverrideLeftOrRight",
          "overrideAnyDirection">: gql "SimplifiedOverrideAnyDirection"],

-- simplifiedOverrideLeft
--     : LEFT_ANGLE_BRACKET simplifiedSecondary
--     ;
      def "SimplifiedOverrideLeft" $
        gql "SimplifiedSecondary",

-- simplifiedOverrideUndirected
--     : TILDE simplifiedSecondary
--     ;
      def "SimplifiedOverrideUndirected" $
        gql "SimplifiedSecondary",

-- simplifiedOverrideRight
--     : simplifiedSecondary RIGHT_ANGLE_BRACKET
--     ;
      def "SimplifiedOverrideRight" $
        gql "SimplifiedSecondary",

-- simplifiedOverrideLeftOrUndirected
--     : LEFT_ARROW_TILDE simplifiedSecondary
--     ;
      def "SimplifiedOverrideLeftOrUndirected" $
        gql "SimplifiedSecondary",

-- simplifiedOverrideUndirectedOrRight
--     : TILDE simplifiedSecondary RIGHT_ANGLE_BRACKET
--     ;
      def "SimplifiedOverrideUndirectedOrRight" $
        gql "SimplifiedSecondary",

-- simplifiedOverrideLeftOrRight
--     : LEFT_ANGLE_BRACKET simplifiedSecondary RIGHT_ANGLE_BRACKET
--     ;
      def "SimplifiedOverrideLeftOrRight" $
        gql "SimplifiedSecondary",

-- simplifiedOverrideAnyDirection
--     : MINUS_SIGN simplifiedSecondary
--     ;
      def "SimplifiedOverrideAnyDirection" $
        gql "SimplifiedSecondary",

-- simplifiedSecondary
--     : simplifiedPrimary
--     | simplifiedNegation
--     ;
      def "SimplifiedSecondary" $
        union [
          "primary">: gql "SimplifiedPrimary",
          "negation">: gql "SimplifiedNegation"],

-- simplifiedNegation
--     : EXCLAMATION_MARK simplifiedPrimary
--     ;
      def "SimplifiedNegation" $
        gql "SimplifiedPrimary",

-- simplifiedPrimary
--     : labelName
--     | LEFT_PAREN simplifiedContents RIGHT_PAREN
--     ;
      def "SimplifiedPrimary" $
        union [
          "labelName">: gql "LabelName",
          "parenthesizedContents">: gql "SimplifiedContents"],

-- // 16.13 <where clause>
--
-- whereClause
--     : WHERE searchCondition
--     ;
      def "WhereClause" $
        gql "SearchCondition",

-- // 16.14 <yield clause>
--
-- yieldClause
--     : YIELD yieldItemList
--     ;
      def "YieldClause" $
        gql "YieldItemList",

-- yieldItemList
--     : yieldItem (COMMA yieldItem)*
--     ;
      def "YieldItemList" $
        nonemptyList $ gql "YieldItem",

-- yieldItem
--     : (yieldItemName yieldItemAlias?)
--     ;
      def "YieldItem" $
        record [
          "name">: gql "YieldItemName",
          "alias">: optional $ gql "YieldItemAlias"],

-- yieldItemName
--     : fieldName
--     ;
      def "YieldItemName" $
        gql "FieldName",

-- yieldItemAlias
--     : AS bindingVariable
--     ;
      def "YieldItemAlias" $
        gql "BindingVariable",

-- // 16.15 <group by clause>
--
-- groupByClause
--     : GROUP BY groupingElementList
--     ;
      def "GroupByClause" $
        gql "GroupingElementList",

-- groupingElementList
--     : groupingElement (COMMA groupingElement)*
--     | emptyGroupingSet
--     ;
      def "GroupingElementList" $
        union [
          "elements">: nonemptyList $ gql "GroupingElement",
          "emptySet">: unit],

-- groupingElement
--     : bindingVariableReference
--     ;
      def "GroupingElement" $
        gql "BindingVariableReference",

-- emptyGroupingSet
--     : LEFT_PAREN RIGHT_PAREN
--     ;
      -- This is already represented as a unit type in GroupingElementList

-- // 16.16 <order by clause>
--
-- orderByClause
--     : ORDER BY sortSpecificationList
--     ;
      def "OrderByClause" $
        gql "SortSpecificationList",

-- // 16.17 <sort specification list>
--
-- sortSpecificationList
--     : sortSpecification (COMMA sortSpecification)*
--     ;
      def "SortSpecificationList" $
        nonemptyList $ gql "SortSpecification",

-- sortSpecification
--     : sortKey orderingSpecification? nullOrdering?
--     ;
      def "SortSpecification" $
        record [
          "sortKey">: gql "SortKey",
          "ordering">: optional $ gql "OrderingSpecification",
          "nullOrdering">: optional $ gql "NullOrdering"],

-- sortKey
--     : aggregatingValueExpression
--     ;
      def "SortKey" $
        gql "AggregatingValueExpression",

-- orderingSpecification
--     : ASC
--     | ASCENDING
--     | DESC
--     | DESCENDING
--     ;
      def "OrderingSpecification" $
        enum ["ascending", "descending"],

-- nullOrdering
--     : NULLS FIRST
--     | NULLS LAST
--     ;
      def "NullOrdering" $
        enum ["nullsFirst", "nullsLast"],

-- // 16.18 <limit clause>
--
-- limitClause
--     : LIMIT nonNegativeIntegerSpecification
--     ;
      def "LimitClause" $
        gql "NonNegativeIntegerSpecification",

-- // 16.19 <offset clause>
--
-- offsetClause
--     : offsetSynonym nonNegativeIntegerSpecification
--     ;
      def "OffsetClause" $
        record [
          "synonym">: gql "OffsetSynonym",
          "value">: gql "NonNegativeIntegerSpecification"],

-- offsetSynonym
--     : OFFSET
--     | SKIP_RESERVED_WORD
--     ;
      def "OffsetSynonym" $
        enum ["offset", "skipReservedWord"],

-- // 17.1 <schema reference> and <catalog schema parent name>
--
-- schemaReference
--     : absoluteCatalogSchemaReference
--     | relativeCatalogSchemaReference
--     | referenceParameterSpecification
--     ;
      def "SchemaReference" $
        union [
          "absoluteReference">: gql "AbsoluteCatalogSchemaReference",
          "relativeReference">: gql "RelativeCatalogSchemaReference",
          "parameterSpecification">: gql "ReferenceParameterSpecification"],

-- absoluteCatalogSchemaReference
--     : SOLIDUS
--     | absoluteDirectoryPath schemaName
--     ;
      def "AbsoluteCatalogSchemaReference" $
        union [
          "root">: unit,
          "directoryAndSchema">: gql "AbsoluteDirectoryAndSchema"],

      def "AbsoluteDirectoryAndSchema" $
        record [
          "directoryPath">: gql "AbsoluteDirectoryPath",
          "schemaName">: gql "SchemaName"],

-- catalogSchemaParentAndName
--     : absoluteDirectoryPath schemaName
--     ;
      def "CatalogSchemaParentAndName" $
        gql "AbsoluteDirectoryAndSchema",

-- relativeCatalogSchemaReference
--     : predefinedSchemaReference
--     | relativeDirectoryPath schemaName
--     ;
      def "RelativeCatalogSchemaReference" $
        union [
          "predefinedReference">: gql "PredefinedSchemaReference",
          "directoryAndSchema">: gql "RelativeDirectoryAndSchema"],

      def "RelativeDirectoryAndSchema" $
        record [
          "directoryPath">: gql "RelativeDirectoryPath",
          "schemaName">: gql "SchemaName"],

-- predefinedSchemaReference
--     : HOME_SCHEMA
--     | CURRENT_SCHEMA
--     | PERIOD
--     ;
      def "PredefinedSchemaReference" $
        enum ["homeSchema", "currentSchema", "period"],

-- absoluteDirectoryPath
--     : SOLIDUS simpleDirectoryPath?
--     ;
      def "AbsoluteDirectoryPath" $
        optional $ gql "SimpleDirectoryPath",

-- relativeDirectoryPath
--     : DOUBLE_PERIOD (SOLIDUS DOUBLE_PERIOD)* SOLIDUS simpleDirectoryPath?
--     ;
      def "RelativeDirectoryPath" $
        record [
          "parentDirectories">: nonNegativeInt32,
          "simplePath">: optional $ gql "SimpleDirectoryPath"],

-- simpleDirectoryPath
--     : (directoryName SOLIDUS)+
--     ;
      def "SimpleDirectoryPath" $
        nonemptyList $ gql "DirectoryName",

-- // 17.2 <graph reference> and <catalog graph parent and name>
--
-- graphReference
--     : catalogObjectParentReference graphName
--     | delimitedGraphName
--     | homeGraph
--     | referenceParameterSpecification
--     ;
      def "GraphReference" $
        union [
          "parentAndGraphName">: gql "ParentAndGraphName",
          "delimitedGraphName">: gql "DelimitedGraphName",
          "homeGraph">: gql "HomeGraph",
          "parameterSpecification">: gql "ReferenceParameterSpecification"],

      def "ParentAndGraphName" $
        record [
          "parentReference">: gql "CatalogObjectParentReference",
          "graphName">: gql "GraphName"],

-- catalogGraphParentAndName
--     : catalogObjectParentReference? graphName
--     ;
      def "CatalogGraphParentAndName" $
        record [
          "parentReference">: optional $ gql "CatalogObjectParentReference",
          "graphName">: gql "GraphName"],

-- homeGraph
--     : HOME_PROPERTY_GRAPH
--     | HOME_GRAPH
--     ;
      def "HomeGraph" $
        enum ["homePropertyGraph", "homeGraph"],

-- // 17.3 <graph type reference> and <catalog graph type parent and name>
--
-- graphTypeReference
--     : catalogGraphTypeParentAndName
--     | referenceParameterSpecification
--     ;
      def "GraphTypeReference" $
        union [
          "parentAndTypeName">: gql "CatalogGraphTypeParentAndName",
          "parameterSpecification">: gql "ReferenceParameterSpecification"],

-- catalogGraphTypeParentAndName
--     : catalogObjectParentReference? graphTypeName
--     ;
      def "CatalogGraphTypeParentAndName" $
        record [
          "parentReference">: optional $ gql "CatalogObjectParentReference",
          "graphTypeName">: gql "GraphTypeName"],

-- // 17.4 <binding table reference> and <catalog binding table parent name>
--
-- bindingTableReference
--     : catalogObjectParentReference bindingTableName
--     | delimitedBindingTableName
--     | referenceParameterSpecification
--     ;
      def "BindingTableReference" $
        union [
          "parentAndTableName">: gql "ParentAndTableName",
          "delimitedBindingTableName">: gql "DelimitedBindingTableName",
          "parameterSpecification">: gql "ReferenceParameterSpecification"],

      def "ParentAndTableName" $
        record [
          "parentReference">: gql "CatalogObjectParentReference",
          "tableName">: gql "BindingTableName"],

-- // 17.5 <procedure reference> and <catalog procedure parent and name>
--
-- procedureReference
--     : catalogProcedureParentAndName
--     | referenceParameterSpecification
--     ;
      def "ProcedureReference" $
        union [
          "parentAndProcedureName">: gql "CatalogProcedureParentAndName",
          "parameterSpecification">: gql "ReferenceParameterSpecification"],

-- catalogProcedureParentAndName
--     : catalogObjectParentReference? procedureName
--     ;
      def "CatalogProcedureParentAndName" $
        record [
          "parentReference">: optional $ gql "CatalogObjectParentReference",
          "procedureName">: gql "ProcedureName"],

-- // 17.6 <catalog object parent reference>
--
-- catalogObjectParentReference
--     : schemaReference SOLIDUS? (objectName PERIOD)*
--     | (objectName PERIOD)+
--     ;
      def "CatalogObjectParentReference" $
        union [
          "schemaAndObjects">: gql "SchemaAndObjects",
          "objectsOnly">: nonemptyList $ gql "ObjectName"],

      def "SchemaAndObjects" $
        record [
          "schemaReference">: gql "SchemaReference",
          "objects">: list $ gql "ObjectName"],

-- // 17.7 <reference parameter specification>
--
-- referenceParameterSpecification
--     : SUBSTITUTED_PARAMETER_REFERENCE
--     ;
      def "ReferenceParameterSpecification" $
        unit,

-- // 18.1 <nested graph type specification>
--
-- nestedGraphTypeSpecification
--     : LEFT_BRACE graphTypeSpecificationBody RIGHT_BRACE
--     ;
      def "NestedGraphTypeSpecification" $
        gql "GraphTypeSpecificationBody",

-- graphTypeSpecificationBody
--     : elementTypeList
--     ;
      def "GraphTypeSpecificationBody" $
        gql "ElementTypeList",

-- elementTypeList
--     : elementTypeSpecification (COMMA elementTypeSpecification)*
--     ;
      def "ElementTypeList" $
        nonemptyList $ gql "ElementTypeSpecification",

-- elementTypeSpecification
--     : nodeTypeSpecification
--     | edgeTypeSpecification
--     ;
      def "ElementTypeSpecification" $
        union [
          "nodeType">: gql "NodeTypeSpecification",
          "edgeType">: gql "EdgeTypeSpecification"],

-- // 18.2 <node type specification>
--
-- nodeTypeSpecification
--     : nodeTypePattern
--     | nodeTypePhrase
--     ;
      def "NodeTypeSpecification" $
        union [
          "pattern">: gql "NodeTypePattern",
          "phrase">: gql "NodeTypePhrase"],

-- nodeTypePattern
--     : (nodeSynonym TYPE? nodeTypeName)? LEFT_PAREN localNodeTypeAlias? nodeTypeFiller? RIGHT_PAREN
--     ;
      def "NodeTypePattern" $
        record [
          "synonymAndTypeName">: optional $ gql "NodeSynonymAndTypeName",
          "alias">: optional $ gql "LocalNodeTypeAlias",
          "filler">: optional $ gql "NodeTypeFiller"],

      def "NodeSynonymAndTypeName" $
        record [
          "nodeSynonym">: gql "NodeSynonym",
          "typeName">: optional $ gql "NodeTypeName"],

-- nodeTypePhrase
--     : nodeSynonym TYPE? nodeTypePhraseFiller (AS localNodeTypeAlias)?
--     ;
      def "NodeTypePhrase" $
        record [
          "synonym">: gql "NodeSynonym",
          "typePhraseFiller">: gql "NodeTypePhraseFiller",
          "alias">: optional $ gql "LocalNodeTypeAlias"],

-- nodeTypePhraseFiller
--     : nodeTypeName nodeTypeFiller?
--     | nodeTypeFiller
--     ;
      def "NodeTypePhraseFiller" $
        union [
          "typeName">: gql "NodeTypeNameWithFiller",
          "fillerOnly">: gql "NodeTypeFiller"],

      def "NodeTypeNameWithFiller" $
        record [
          "typeName">: gql "NodeTypeName",
          "filler">: optional $ gql "NodeTypeFiller"],

-- nodeTypeFiller
--     : nodeTypeKeyLabelSet nodeTypeImpliedContent?
--     | nodeTypeImpliedContent
--     ;
      def "NodeTypeFiller" $
        union [
          "keyLabelSet">: gql "NodeKeyLabelSetWithContent",
          "impliedContent">: gql "NodeTypeImpliedContent"],

      def "NodeKeyLabelSetWithContent" $
        record [
          "keyLabelSet">: gql "NodeTypeKeyLabelSet",
          "impliedContent">: optional $ gql "NodeTypeImpliedContent"],

-- localNodeTypeAlias
--     : regularIdentifier
--     ;
      def "LocalNodeTypeAlias" $
        string,

-- nodeTypeImpliedContent
--     : nodeTypeLabelSet
--     | nodeTypePropertyTypes
--     | nodeTypeLabelSet nodeTypePropertyTypes
--     ;
      def "NodeTypeImpliedContent" $
        union [
          "labelSet">: gql "NodeTypeLabelSet",
          "propertyTypes">: gql "NodeTypePropertyTypes",
          "labelSetWithProperties">: gql "NodeLabelSetWithProperties"],

      def "NodeLabelSetWithProperties" $
        record [
          "labelSet">: gql "NodeTypeLabelSet",
          "propertyTypes">: gql "NodeTypePropertyTypes"],

-- nodeTypeKeyLabelSet
--     : labelSetPhrase? IMPLIES
--     ;
      def "NodeTypeKeyLabelSet" $
        optional $ gql "LabelSetPhrase",

-- nodeTypeLabelSet
--     : labelSetPhrase
--     ;
      def "NodeTypeLabelSet" $
        gql "LabelSetPhrase",

-- nodeTypePropertyTypes
--     : propertyTypesSpecification
--     ;
      def "NodeTypePropertyTypes" $
        gql "PropertyTypesSpecification",

-- // 18.3 <edge type specification>
--
-- edgeTypeSpecification
--     : edgeTypePattern
--     | edgeTypePhrase
--     ;
      def "EdgeTypeSpecification" $
        union [
          "pattern">: gql "EdgeTypePattern",
          "phrase">: gql "EdgeTypePhrase"],

-- edgeTypePattern
--     : (edgeKind? edgeSynonym TYPE? edgeTypeName)? (edgeTypePatternDirected | edgeTypePatternUndirected)
--     ;
      def "EdgeTypePattern" $
        record [
          "kindAndSynonym">: optional $ gql "EdgeKindAndSynonym",
          "patternType">: gql "EdgeTypePatternType"],

      def "EdgeKindAndSynonym" $
        record [
          "kind">: optional $ gql "EdgeKind",
          "synonym">: gql "EdgeSynonym",
          "typeName">: optional $ gql "EdgeTypeName"],

      def "EdgeTypePatternType" $
        union [
          "directed">: gql "EdgeTypePatternDirected",
          "undirected">: gql "EdgeTypePatternUndirected"],

-- edgeTypePhrase
--     : edgeKind edgeSynonym TYPE? edgeTypePhraseFiller endpointPairPhrase
--     ;
      def "EdgeTypePhrase" $
        record [
          "kind">: gql "EdgeKind",
          "synonym">: gql "EdgeSynonym",
          "typeNameAndFiller">: gql "EdgeTypePhraseFiller",
          "endpointPair">: gql "EndpointPairPhrase"],

-- edgeTypePhraseFiller
--     : edgeTypeName edgeTypeFiller?
--     | edgeTypeFiller
--     ;
      def "EdgeTypePhraseFiller" $
        union [
          "typeNameWithFiller">: gql "EdgeTypeNameWithFiller",
          "fillerOnly">: gql "EdgeTypeFiller"],

      def "EdgeTypeNameWithFiller" $
        record [
          "typeName">: gql "EdgeTypeName",
          "filler">: optional $ gql "EdgeTypeFiller"],

-- edgeTypeFiller
--     : edgeTypeKeyLabelSet edgeTypeImpliedContent?
--     | edgeTypeImpliedContent
--     ;
      def "EdgeTypeFiller" $
        union [
          "keyLabelSetWithContent">: gql "EdgeKeyLabelSetWithContent",
          "impliedContent">: gql "EdgeTypeImpliedContent"],

      def "EdgeKeyLabelSetWithContent" $
        record [
          "keyLabelSet">: gql "EdgeTypeKeyLabelSet",
          "impliedContent">: optional $ gql "EdgeTypeImpliedContent"],

-- edgeTypeImpliedContent
--     : edgeTypeLabelSet
--     | edgeTypePropertyTypes
--     | edgeTypeLabelSet edgeTypePropertyTypes
--     ;
      def "EdgeTypeImpliedContent" $
        union [
          "labelSet">: gql "EdgeTypeLabelSet",
          "propertyTypes">: gql "EdgeTypePropertyTypes",
          "labelSetWithProperties">: gql "EdgeLabelSetWithProperties"],

      def "EdgeLabelSetWithProperties" $
        record [
          "labelSet">: gql "EdgeTypeLabelSet",
          "propertyTypes">: gql "EdgeTypePropertyTypes"],

-- edgeTypeKeyLabelSet
--     : labelSetPhrase? IMPLIES
--     ;
      def "EdgeTypeKeyLabelSet" $
        optional $ gql "LabelSetPhrase",

-- edgeTypeLabelSet
--     : labelSetPhrase
--     ;
      def "EdgeTypeLabelSet" $
        gql "LabelSetPhrase",

-- edgeTypePropertyTypes
--     : propertyTypesSpecification
--     ;
      def "EdgeTypePropertyTypes" $
        gql "PropertyTypesSpecification",

-- edgeTypePatternDirected
--     : edgeTypePatternPointingRight
--     | edgeTypePatternPointingLeft
--     ;
      def "EdgeTypePatternDirected" $
        union [
          "pointingRight">: gql "EdgeTypePatternPointingRight",
          "pointingLeft">: gql "EdgeTypePatternPointingLeft"],

-- edgeTypePatternPointingRight
--     : sourceNodeTypeReference arcTypePointingRight destinationNodeTypeReference
--     ;
      def "EdgeTypePatternPointingRight" $
        record [
          "source">: gql "SourceNodeTypeReference",
          "arc">: gql "ArcTypePointingRight",
          "destination">: gql "DestinationNodeTypeReference"],

-- edgeTypePatternPointingLeft
--     : destinationNodeTypeReference arcTypePointingLeft sourceNodeTypeReference
--     ;
      def "EdgeTypePatternPointingLeft" $
        record [
          "destination">: gql "DestinationNodeTypeReference",
          "arc">: gql "ArcTypePointingLeft",
          "source">: gql "SourceNodeTypeReference"],

-- edgeTypePatternUndirected
--     : sourceNodeTypeReference arcTypeUndirected destinationNodeTypeReference
--     ;
      def "EdgeTypePatternUndirected" $
        record [
          "source">: gql "SourceNodeTypeReference",
          "arc">: gql "ArcTypeUndirected",
          "destination">: gql "DestinationNodeTypeReference"],

-- arcTypePointingRight
--     : MINUS_LEFT_BRACKET edgeTypeFiller BRACKET_RIGHT_ARROW
--     ;
      def "ArcTypePointingRight" $
        gql "EdgeTypeFiller",

-- arcTypePointingLeft
--     : LEFT_ARROW_BRACKET edgeTypeFiller RIGHT_BRACKET_MINUS
--     ;
      def "ArcTypePointingLeft" $
        gql "EdgeTypeFiller",

-- arcTypeUndirected
--     : TILDE_LEFT_BRACKET edgeTypeFiller RIGHT_BRACKET_TILDE
--     ;
      def "ArcTypeUndirected" $
        gql "EdgeTypeFiller",

-- sourceNodeTypeReference
--     : LEFT_PAREN sourceNodeTypeAlias RIGHT_PAREN
--     | LEFT_PAREN nodeTypeFiller? RIGHT_PAREN
--     ;
      def "SourceNodeTypeReference" $
        union [
          "alias">: gql "SourceNodeTypeAlias",
          "filler">: optional $ gql "NodeTypeFiller"],

-- destinationNodeTypeReference
--     : LEFT_PAREN destinationNodeTypeAlias RIGHT_PAREN
--     | LEFT_PAREN nodeTypeFiller? RIGHT_PAREN
--     ;
      def "DestinationNodeTypeReference" $
        union [
          "alias">: gql "DestinationNodeTypeAlias",
          "filler">: optional $ gql "NodeTypeFiller"],

-- edgeKind
--     : DIRECTED
--     | UNDIRECTED
--     ;
      def "EdgeKind" $
        enum ["directed", "undirected"],

-- endpointPairPhrase
--     : CONNECTING endpointPair
--     ;
      def "EndpointPairPhrase" $
        gql "EndpointPair",

-- endpointPair
--     : endpointPairDirected
--     | endpointPairUndirected
--     ;
      def "EndpointPair" $
        union [
          "directedPair">: gql "EndpointPairDirected",
          "undirectedPair">: gql "EndpointPairUndirected"],

-- endpointPairDirected
--     : endpointPairPointingRight
--     | endpointPairPointingLeft
--     ;
      def "EndpointPairDirected" $
        union [
          "pointingRight">: gql "EndpointPairPointingRight",
          "pointingLeft">: gql "EndpointPairPointingLeft"],

-- endpointPairPointingRight
--     : LEFT_PAREN sourceNodeTypeAlias connectorPointingRight destinationNodeTypeAlias RIGHT_PAREN
--     ;
      def "EndpointPairPointingRight" $
        record [
          "sourceAlias">: gql "SourceNodeTypeAlias",
          "connector">: gql "ConnectorPointingRight",
          "destinationAlias">: gql "DestinationNodeTypeAlias"],

-- endpointPairPointingLeft
--     : LEFT_PAREN destinationNodeTypeAlias LEFT_ARROW sourceNodeTypeAlias RIGHT_PAREN
--     ;
      def "EndpointPairPointingLeft" $
        record [
          "destinationAlias">: gql "DestinationNodeTypeAlias",
          "sourceAlias">: gql "SourceNodeTypeAlias"],

-- endpointPairUndirected
--     : LEFT_PAREN sourceNodeTypeAlias connectorUndirected destinationNodeTypeAlias RIGHT_PAREN
--     ;
      def "EndpointPairUndirected" $
        record [
          "sourceAlias">: gql "SourceNodeTypeAlias",
          "connector">: gql "ConnectorUndirected",
          "destinationAlias">: gql "DestinationNodeTypeAlias"],

-- connectorPointingRight
--     : TO
--     | RIGHT_ARROW
--     ;
      def "ConnectorPointingRight" $
        enum ["to", "rightArrow"],

-- connectorUndirected
--     : TO
--     | TILDE
--     ;
      def "ConnectorUndirected" $
        enum ["to", "tilde"],

-- sourceNodeTypeAlias
--     : regularIdentifier
--     ;
      def "SourceNodeTypeAlias"
        string,

-- destinationNodeTypeAlias
--     : regularIdentifier
--     ;
      def "DestinationNodeTypeAlias"
        string,

-- // 18.4 <label set phrase> and <label set specification>
--
-- labelSetPhrase
--     : LABEL labelName
--     | LABELS labelSetSpecification
--     | isOrColon labelSetSpecification
--     ;
      def "LabelSetPhrase" $
        union [
          "singleLabel">: gql "LabelName",
          "multipleLabels">: gql "LabelSetSpecification",
          "isOrColonWithLabels">: gql "IsOrColonWithLabels"],

      def "IsOrColonWithLabels" $
        record [
          "isOrColon">: gql "IsOrColon",
          "labels">: gql "LabelSetSpecification"],

-- labelSetSpecification
--     : labelName (AMPERSAND labelName)*
--     ;
      def "LabelSetSpecification" $
        nonemptyList $ gql "LabelName",

-- // 18.5 <property types specification>
--
-- propertyTypesSpecification
--     : LEFT_BRACE propertyTypeList? RIGHT_BRACE
--     ;
      def "PropertyTypesSpecification" $
        optional $ gql "PropertyTypeList",

-- propertyTypeList
--     : propertyType (COMMA propertyType)*
--     ;
      def "PropertyTypeList" $
        nonemptyList $ gql "PropertyType",

-- // 18.6 <property type>
--
-- propertyType
--     : propertyName typed? propertyValueType
--     ;
      def "PropertyType" $
        record [
          "name">: gql "PropertyName",
          "typed">: optional $ gql "Typed",
          "valueType">: gql "PropertyValueType"],

-- // 18.7 <property value type>
--
-- propertyValueType
--     : valueType
--     ;
      def "PropertyValueType" $
        gql "ValueType",

-- // 18.8 <binding table type>
--
-- bindingTableType
--     : BINDING? TABLE fieldTypesSpecification
--     ;
      def "BindingTableType" $
        record [
          "binding">: boolean,
          "fieldTypes">: gql "FieldTypesSpecification"],

-- // 18.9 <value type>
--
-- valueType
--     : predefinedType                                                                                                                              #predefinedTypeLabel
--     | pathValueType                                                                                                                               #pathValueTypeLabel
--     | listValueTypeName LEFT_ANGLE_BRACKET valueType RIGHT_ANGLE_BRACKET (LEFT_BRACKET maxLength RIGHT_BRACKET)? notNull?     #listValueTypeAlt1
--     | valueType listValueTypeName (LEFT_BRACKET maxLength RIGHT_BRACKET)? notNull?                                                    #listValueTypeAlt2
--     | listValueTypeName (LEFT_BRACKET maxLength RIGHT_BRACKET)? notNull?                                                                #listValueTypeAlt3
--     | recordType                                                                                                                                   #recordTypeLabel
--     | ANY VALUE? notNull?                                                                                                                        #openDynamicUnionTypeLabel
--     | ANY? PROPERTY VALUE notNull?                                                                                                             #dynamicPropertyValueTypeLabel
--     | ANY VALUE? LEFT_ANGLE_BRACKET valueType (VERTICAL_BAR valueType)* RIGHT_ANGLE_BRACKET                                         #closedDynamicUnionTypeAtl1
--     | valueType VERTICAL_BAR valueType                                                                                                        #closedDynamicUnionTypeAtl2
--     ;
      def "ValueType" $
        union [
          "predefinedType">: gql "PredefinedType",
          "pathValueType">: gql "PathValueType",
          "listValueTypeAlt1">: gql "ListValueTypeAlt1",
          "listValueTypeAlt2">: gql "ListValueTypeAlt2",
          "listValueTypeAlt3">: gql "ListValueTypeAlt3",
          "recordType">: gql "RecordType",
          "openDynamicUnionType">: gql "OpenDynamicUnionType",
          "dynamicPropertyValueType">: gql "DynamicPropertyValueType",
          "closedDynamicUnionTypeAlt1">: gql "ClosedDynamicUnionTypeAlt1",
          "closedDynamicUnionTypeAlt2">: gql "ClosedDynamicUnionTypeAlt2"],

      def "ListValueTypeAlt1" $
        record [
          "typeName">: gql "ListValueTypeName",
          "valueType">: gql "ValueType",
          "maxLength">: optional $ gql "MaxLength",
          "notNull">: boolean],

      def "ListValueTypeAlt2" $
        record [
          "valueType">: gql "ValueType",
          "typeName">: gql "ListValueTypeName",
          "maxLength">: optional $ gql "MaxLength",
          "notNull">: boolean],

      def "ListValueTypeAlt3" $
        record [
          "typeName">: gql "ListValueTypeName",
          "maxLength">: optional $ gql "MaxLength",
          "notNull">: boolean],

      def "OpenDynamicUnionType" $
        record [
          "value">: boolean,
          "notNull">: boolean],

      def "DynamicPropertyValueType" $
        record [
          "any">: optional boolean,
          "property">: unit,
          "value">: unit,
          "notNull">: boolean],

      def "ClosedDynamicUnionTypeAlt1" $
        record [
          "anyValue">: optional boolean,
          "valueTypes">: nonemptyList $ gql "ValueType"],

      def "ClosedDynamicUnionTypeAlt2" $
        record [
          "valueTypes">: nonemptyList $ gql "ValueType"],

-- typed
--     : DOUBLE_COLON
--     | TYPED
--     ;
      def "Typed" $
        unit,

-- predefinedType
--     : booleanType
--     | characterStringType
--     | byteStringType
--     | numericType
--     | temporalType
--     | referenceValueType
--     | immaterialValueType
--     ;
      def "PredefinedType" $
        union [
          "booleanType">: gql "BooleanType",
          "characterStringType">: gql "CharacterStringType",
          "byteStringType">: gql "ByteStringType",
          "numericType">: gql "NumericType",
          "temporalType">: gql "TemporalType",
          "referenceValueType">: gql "ReferenceValueType",
          "immaterialValueType">: gql "ImmaterialValueType"],

-- booleanType
--     : (BOOL | BOOLEAN) notNull?
--     ;
      def "BooleanType" $
        record [
          "notNull">: boolean],

-- characterStringType
--     : STRING (LEFT_PAREN (minLength COMMA)? maxLength RIGHT_PAREN)? notNull?
--     | CHAR (LEFT_PAREN fixedLength RIGHT_PAREN)? notNull?
--     | VARCHAR (LEFT_PAREN maxLength RIGHT_PAREN)? notNull?
--     ;
      def "CharacterStringType" $
        union [
          "stringType">: gql "StringType",
          "charType">: gql "CharType",
          "varcharType">: gql "VarcharType"],

      def "StringType" $
        record [
          "minLength">: optional $ gql "MinLength",
          "maxLength">: optional $ gql "MaxLength",
          "notNull">: boolean],

      def "CharType" $
        record [
          "fixedLength">: optional $ gql "FixedLength",
          "notNull">: boolean],

      def "VarcharType" $
        record [
          "maxLength">: optional $ gql "MaxLength",
          "notNull">: boolean],

-- byteStringType
--     : BYTES (LEFT_PAREN (minLength COMMA)? maxLength RIGHT_PAREN)? notNull?
--     | BINARY (LEFT_PAREN fixedLength RIGHT_PAREN)? notNull?
--     | VARBINARY (LEFT_PAREN maxLength RIGHT_PAREN)? notNull?
--     ;
      def "ByteStringType" $
        union [
          "bytesType">: gql "BytesType",
          "binaryType">: gql "BinaryType",
          "varbinaryType">: gql "VarbinaryType"],

      def "BytesType" $
        record [
          "minLength">: optional $ gql "MinLength",
          "maxLength">: optional $ gql "MaxLength",
          "notNull">: boolean],

      def "BinaryType" $
        record [
          "fixedLength">: optional $ gql "FixedLength",
          "notNull">: boolean],

      def "VarbinaryType" $
        record [
          "maxLength">: optional $ gql "MaxLength",
          "notNull">: boolean],

-- minLength
--     : unsignedInteger
--     ;
      def "MinLength" $
        gql "UnsignedInteger",

-- maxLength
--     : unsignedInteger
--     ;

-- fixedLength
--     : unsignedInteger
--     ;
      def "FixedLength" $
        gql "UnsignedInteger",

-- minLength
--     : unsignedInteger
--     ;

-- maxLength
--     : unsignedInteger
--     ;
      def "MaxLength" $
        gql "UnsignedInteger",

-- fixedLength
--     : unsignedInteger
--     ;

-- numericType
--     : exactNumericType
--     | approximateNumericType
--     ;
      def "NumericType" $
        union [
          "exact">: gql "ExactNumericType",
          "approximate">: gql "ApproximateNumericType"],

-- exactNumericType
--     : binaryExactNumericType
--     | decimalExactNumericType
--     ;
      def "ExactNumericType" $
        union [
          "binary">: gql "BinaryExactNumericType",
          "decimal">: gql "DecimalExactNumericType"],

-- binaryExactNumericType
--     : signedBinaryExactNumericType
--     | unsignedBinaryExactNumericType
--     ;
      def "BinaryExactNumericType" $
        union [
          "signed">: gql "SignedBinaryExactNumericType",
          "unsigned">: gql "UnsignedBinaryExactNumericType"],

-- signedBinaryExactNumericType
--     : INT8 notNull?
--     | INT16 notNull?
--     | INT32 notNull?
--     | INT64 notNull?
--     | INT128 notNull?
--     | INT256 notNull?
--     | SMALLINT notNull?
--     | INT (LEFT_PAREN precision RIGHT_PAREN)? notNull?
--     | BIGINT notNull?
--     | SIGNED? verboseBinaryExactNumericType
--     ;
      def "SignedBinaryExactNumericType" $
        union [
          "int8">: gql "Int8Type",
          "int16">: gql "Int16Type",
          "int32">: gql "Int32Type",
          "int64">: gql "Int64Type",
          "int128">: gql "Int128Type",
          "int256">: gql "Int256Type",
          "smallInt">: gql "SmallIntType",
          "intWithPrecision">: gql "IntWithPrecision",
          "bigInt">: gql "BigIntType",
          "signedVerboseType">: gql "SignedVerboseBinaryExactNumericType"],

      def "Int8Type" $
        record ["notNull">: boolean],

      def "Int16Type" $
        record ["notNull">: boolean],

      def "Int32Type" $
        record ["notNull">: boolean],

      def "Int64Type" $
        record ["notNull">: boolean],

      def "Int128Type" $
        record ["notNull">: boolean],

      def "Int256Type" $
        record ["notNull">: boolean],

      def "SmallIntType" $
        record ["notNull">: boolean],

      def "BigIntType" $
        record ["notNull">: boolean],

      def "IntWithPrecision" $
        record [
          "precision">: optional $ gql "Precision",
          "notNull">: boolean],

      def "SignedVerboseBinaryExactNumericType" $
        record [
          "signed">: boolean,
          "verboseType">: gql "VerboseBinaryExactNumericType"],

-- unsignedBinaryExactNumericType
--     : UINT8 notNull?
--     | UINT16 notNull?
--     | UINT32 notNull?
--     | UINT64 notNull?
--     | UINT128 notNull?
--     | UINT256 notNull?
--     | USMALLINT notNull?
--     | UINT (LEFT_PAREN precision RIGHT_PAREN)? notNull?
--     | UBIGINT notNull?
--     | UNSIGNED verboseBinaryExactNumericType
--     ;
      def "UnsignedBinaryExactNumericType" $
        union [
          "uint8">: gql "Uint8Type",
          "uint16">: gql "Uint16Type",
          "uint32">: gql "Uint32Type",
          "uint64">: gql "Uint64Type",
          "uint128">: gql "Uint128Type",
          "uint256">: gql "Uint256Type",
          "uSmallInt">: gql "USmallIntType",
          "uintWithPrecision">: gql "UintWithPrecision",
          "uBigInt">: gql "UBigIntType",
          "unsigned">: gql "VerboseBinaryExactNumericType"],

      def "Uint8Type" $
        record ["notNull">: boolean],

      def "Uint16Type" $
        record ["notNull">: boolean],

      def "Uint32Type" $
        record ["notNull">: boolean],

      def "Uint64Type" $
        record ["notNull">: boolean],

      def "Uint128Type" $
        record ["notNull">: boolean],

      def "Uint256Type" $
        record ["notNull">: boolean],

      def "USmallIntType" $
        record ["notNull">: boolean],

      def "UBigIntType" $
        record ["notNull">: boolean],

      def "UintWithPrecision" $
        record [
          "precision">: optional $ gql "Precision",
          "notNull">: boolean],

-- verboseBinaryExactNumericType
--     : INTEGER8 notNull?
--     | INTEGER16 notNull?
--     | INTEGER32 notNull?
--     | INTEGER64 notNull?
--     | INTEGER128 notNull?
--     | INTEGER256 notNull?
--     | SMALL INTEGER notNull?
--     | INTEGER (LEFT_PAREN precision RIGHT_PAREN)? notNull?
--     | BIG INTEGER notNull?
--     ;
      def "VerboseBinaryExactNumericType" $
        union [
          "integer8">: gql "Integer8Type",
          "integer16">: gql "Integer16Type",
          "integer32">: gql "Integer32Type",
          "integer64">: gql "Integer64Type",
          "integer128">: gql "Integer128Type",
          "integer256">: gql "Integer256Type",
          "smallInteger">: gql "SmallIntegerType",
          "integerWithPrecision">: gql "IntegerWithPrecision",
          "bigInteger">: gql "BigIntegerType"],

      def "Integer8Type" $
        record ["notNull">: boolean],

      def "Integer16Type" $
        record ["notNull">: boolean],

      def "Integer32Type" $
        record ["notNull">: boolean],

      def "Integer64Type" $
        record ["notNull">: boolean],

      def "Integer128Type" $
        record ["notNull">: boolean],

      def "Integer256Type" $
        record ["notNull">: boolean],

      def "SmallIntegerType" $
        record ["notNull">: boolean],

      def "BigIntegerType" $
        record ["notNull">: boolean],

      def "IntegerWithPrecision" $
        record [
          "precision">: optional $ gql "Precision",
          "notNull">: boolean],

-- Precision
--     : unsignedDecimalInteger
--     ;
      def "Precision" $
        gql "UnsignedDecimalInteger",

-- decimalExactNumericType
--     : (DECIMAL | DEC) (LEFT_PAREN precision (COMMA scale)? RIGHT_PAREN notNull?)?
--     ;
      def "DecimalExactNumericType" $
        optional $ gql "PrecisionAndScale",

      def "PrecisionAndScale" $
        record [
          "precision">: gql "Precision",
          "scale">: optional $ gql "Scale",
          "notNull">: boolean],

-- precision
--     : unsignedDecimalInteger
--     ;

-- scale
--     : unsignedDecimalInteger
--     ;
      def "Scale" $
        gql "UnsignedDecimalInteger",

-- approximateNumericType
--     : FLOAT16 notNull?
--     | FLOAT32 notNull?
--     | FLOAT64 notNull?
--     | FLOAT128 notNull?
--     | FLOAT256 notNull?
--     | FLOAT (LEFT_PAREN precision (COMMA scale)? RIGHT_PAREN)? notNull?
--     | REAL notNull?
--     | DOUBLE PRECISION? notNull?
--     ;
      def "ApproximateNumericType" $
        union [
          "float16">: gql "Float16Type",
          "float32">: gql "Float32Type",
          "float64">: gql "Float64Type",
          "float128">: gql "Float128Type",
          "float256">: gql "Float256Type",
          "floatWithPrecision">: gql "FloatTypeWithPrecision",
          "real">: gql "RealType",
          "doubleWithPrecision">: gql "DoubleTypeWithPrecision"],

      def "Float16Type" $
        record [
          "notNull">: boolean],

      def "Float32Type" $
        record [
          "notNull">: boolean],

      def "Float64Type" $
        record [
          "notNull">: boolean],

      def "Float128Type" $
        record [
          "notNull">: boolean],

      def "Float256Type" $
        record [
          "notNull">: boolean],

      def "FloatTypeWithPrecision" $
        record [
          "precisionAndScale">: optional $ gql "PrecisionAndScale",
          "notNull">: boolean],

      def "RealType" $
        record [
          "notNull">: boolean],

      def "DoubleTypeWithPrecision" $
        record [
          "precision">: boolean,
          "notNull">: boolean],

-- temporalType
--     : temporalInstantType
--     | temporalDurationType
--     ;
      def "TemporalType" $
        union [
          "instantType">: gql "TemporalInstantType",
          "durationType">: gql "TemporalDurationType"],

-- temporalInstantType
--     : datetimeType
--     | localdatetimeType
--     | dateType
--     | timeType
--     | localtimeType
--     ;
      def "TemporalInstantType" $
        union [
          "datetimeType">: gql "DatetimeType",
          "localdatetimeType">: gql "LocaldatetimeType",
          "dateType">: gql "DateType",
          "timeType">: gql "TimeType",
          "localtimeType">: gql "LocaltimeType"],

-- datetimeType
--     : ZONED DATETIME notNull?
--     | TIMESTAMP WITH TIME ZONE notNull?
--     ;
      def "DatetimeType" $
        union [
          "zonedDatetime">: gql "ZonedDatetimeType",
          "timestampWithTimeZone">: gql "TimestampWithTimeZoneType"],

      def "ZonedDatetimeType" $
        record [
          "notNull">: boolean],

      def "TimestampWithTimeZoneType" $
        record [
          "notNull">: boolean],

-- localdatetimeType
--     : LOCAL DATETIME notNull?
--     | TIMESTAMP (WITHOUT TIME ZONE)? notNull?
--     ;
      def "LocaldatetimeType" $
        union [
          "localDatetime">: gql "LocalDatetimeType",
          "timestampWithoutTimeZone">: gql "TimestampWithoutTimeZoneType"],

      def "LocalDatetimeType" $
        record [
          "notNull">: boolean],

      def "TimestampWithoutTimeZoneType" $
        record [
          "notNull">: boolean],

-- dateType
--     : DATE notNull?
--     ;
      def "DateType" $
        record [
          "notNull">: boolean],

-- timeType
--     : ZONED TIME notNull?
--     | TIME WITH TIME ZONE notNull?
--     ;
      def "TimeType" $
        union [
          "zonedTime">: gql "ZonedTimeType",
          "timeWithTimeZone">: gql "TimeWithTimeZoneType"],

      def "ZonedTimeType" $
        record [
          "notNull">: boolean],

      def "TimeWithTimeZoneType" $
        record [
          "notNull">: boolean],

-- localtimeType
--     : LOCAL TIME notNull?
--     | TIME WITHOUT TIME ZONE notNull?
--     ;
      def "LocaltimeType" $
        union [
          "localTime">: gql "LocalTimeType",
          "timeWithoutTimeZone">: gql "TimeWithoutTimeZoneType"],

      def "LocalTimeType" $
        record [
          "notNull">: boolean],

      def "TimeWithoutTimeZoneType" $
        record [
          "notNull">: boolean],

-- temporalDurationType
--     : DURATION LEFT_PAREN temporalDurationQualifier RIGHT_PAREN notNull?
--     ;
      def "TemporalDurationType" $
        record [
          "qualifier">: gql "TemporalDurationQualifier",
          "notNull">: boolean],

-- temporalDurationQualifier
--     : YEAR TO MONTH
--     | DAY TO SECOND
--     ;
      def "TemporalDurationQualifier" $
        enum ["yearToMonth", "dayToSecond"],

-- referenceValueType
--     : graphReferenceValueType
--     | bindingTableReferenceValueType
--     | nodeReferenceValueType
--     | edgeReferenceValueType
--     ;
      def "ReferenceValueType" $
        union [
          "graph">: gql "GraphReferenceValueType",
          "bindingTable">: gql "BindingTableReferenceValueType",
          "node">: gql "NodeReferenceValueType",
          "edge">: gql "EdgeReferenceValueType"],

-- immaterialValueType
--     : nullType
--     | emptyType
--     ;
      def "ImmaterialValueType" $
        union [
          "nullType">: gql "NullType",
          "emptyType">: gql "EmptyType"],

-- nullType
--     : NULL_KW
--     ;
      def "NullType"
        unit,

-- emptyType
--     : NULL_KW notNull
--     | NOTHING
--     ;
      def "EmptyType" $
        unit,

-- graphReferenceValueType
--     : openGraphReferenceValueType
--     | closedGraphReferenceValueType
--     ;
      def "GraphReferenceValueType" $
        union [
          "open">: gql "OpenGraphReferenceValueType",
          "closed">: gql "ClosedGraphReferenceValueType"],

-- closedGraphReferenceValueType
--     : PROPERTY? GRAPH nestedGraphTypeSpecification notNull?
--     ;
      def "ClosedGraphReferenceValueType" $
        record [
          "property">: boolean,
          "nestedSpec">: gql "NestedGraphTypeSpecification",
          "notNull">: boolean],

-- openGraphReferenceValueType
--     : ANY PROPERTY? GRAPH notNull?
--     ;
      def "OpenGraphReferenceValueType" $
        record [
          "any">: optional boolean,
          "property">: boolean,
          "notNull">: boolean],

-- bindingTableReferenceValueType
--     : bindingTableType notNull?
--     ;
      def "BindingTableReferenceValueType" $
        record [
          "bindingTableType">: gql "BindingTableType",
          "notNull">: boolean],

-- nodeReferenceValueType
--     : openNodeReferenceValueType
--     | closedNodeReferenceValueType
--     ;
      def "NodeReferenceValueType" $
        union [
          "open">: gql "OpenNodeReferenceValueType",
          "closed">: gql "ClosedNodeReferenceValueType"],

-- closedNodeReferenceValueType
--     : nodeTypeSpecification notNull?
--     ;
      def "ClosedNodeReferenceValueType" $
        record [
          "nodeTypeSpec">: gql "NodeTypeSpecification",
          "notNull">: boolean],

-- openNodeReferenceValueType
--     : ANY? nodeSynonym notNull?
--     ;
      def "OpenNodeReferenceValueType" $
        record [
          "any">: boolean,
          "nodeSynonym">: gql "NodeSynonym",
          "notNull">: boolean],

-- edgeReferenceValueType
--     : openEdgeReferenceValueType
--     | closedEdgeReferenceValueType
--     ;
      def "EdgeReferenceValueType" $
        union [
          "open">: gql "OpenEdgeReferenceValueType",
          "closed">: gql "ClosedEdgeReferenceValueType"],

-- closedEdgeReferenceValueType
--     : edgeTypeSpecification notNull?
--     ;
      def "ClosedEdgeReferenceValueType" $
        record [
          "edgeTypeSpec">: gql "EdgeTypeSpecification",
          "notNull">: boolean],

-- openEdgeReferenceValueType
--     : ANY? edgeSynonym notNull?
--     ;
      def "OpenEdgeReferenceValueType" $
        record [
          "any">: boolean,
          "edgeSynonym">: gql "EdgeSynonym",
          "notNull">: boolean],

-- pathValueType
--     : PATH notNull?
--     ;
      def "PathValueType" $
        record [
          "notNull">: boolean],

-- listValueTypeName
--     : GROUP? listValueTypeNameSynonym
--     ;
      def "ListValueTypeName" $
        record [
          "group">: boolean,
          "synonym">: gql "ListValueTypeNameSynonym"],

-- listValueTypeNameSynonym
--     : LIST
--     | ARRAY
--     ;
      def "ListValueTypeNameSynonym" $
        enum ["list", "array"],

-- recordType
--     : ANY? RECORD notNull?
--     | RECORD? fieldTypesSpecification notNull?
--     ;
      def "RecordType" $
        union [
          "anyRecord">: gql "AnyRecordType",
          "specifiedRecord">: gql "SpecifiedRecordType"],

      def "AnyRecordType" $
        record [
          "any">: boolean,
          "notNull">: boolean],

      def "SpecifiedRecordType" $
        record [
          "record">: boolean,
          "fieldTypes">: gql "FieldTypesSpecification",
          "notNull">: boolean],

-- fieldTypesSpecification
--     : LEFT_BRACE fieldTypeList? RIGHT_BRACE
--     ;
      def "FieldTypesSpecification" $
        optional $ gql "FieldTypeList",

-- fieldTypeList
--     : fieldType (COMMA fieldType)*
--     ;
      def "FieldTypeList" $
        nonemptyList $ gql "FieldType",

-- notNull
--     : NOT NULL_KW
--     ;
      def "NotNull" $
        unit,

-- // 18.10 <field type>
--
-- fieldType
--     : fieldName typed? valueType
--     ;
      def "FieldType" $
        record [
          "fieldName">: gql "FieldName",
          "typed">: optional $ gql "Typed",
          "valueType">: gql "ValueType"],

-- // 19.1 <search condition>
--
-- searchCondition
--     : booleanValueExpression
--     ;
      def "SearchCondition" $
        gql "BooleanValueExpression",

-- // 19.2 <predicate>
--
-- predicate
--     : existsPredicate
--     | nullPredicate
--     | valueTypePredicate
--     | directedPredicate
--     | labeledPredicate
--     | sourceDestinationPredicate
--     | all_differentPredicate
--     | samePredicate
--     | property_existsPredicate
--     ;
      def "Predicate" $
        union [
          "existsPredicate">: gql "ExistsPredicate",
          "nullPredicate">: gql "NullPredicate",
          "valueTypePredicate">: gql "ValueTypePredicate",
          "directedPredicate">: gql "DirectedPredicate",
          "labeledPredicate">: gql "LabeledPredicate",
          "sourceDestinationPredicate">: gql "SourceDestinationPredicate",
          "allDifferentPredicate">: gql "AllDifferentPredicate",
          "samePredicate">: gql "SamePredicate",
          "propertyExistsPredicate">: gql "PropertyExistsPredicate"],

-- // 19.3 <comparison predicate>
--
-- // The <comparison predicate> productions moved to valueExpression
-- // to avoid left mutually recursive productions.
--
-- comparisonPredicatePart2
--     : compOp valueExpression
--     ;
      def "ComparisonPredicatePart2" $
        record [
          "compOp">: gql "CompOp",
          "valueExpression">: gql "ValueExpression"],

-- compOp
--     : EQUALS_OPERATOR
--     | NOT_EQUALS_OPERATOR
--     | LEFT_ANGLE_BRACKET
--     | RIGHT_ANGLE_BRACKET
--     | LESS_THAN_OR_EQUALS_OPERATOR
--     | GREATER_THAN_OR_EQUALS_OPERATOR
--     ;
      def "CompOp" $
        enum [
          "equals",
          "notEquals",
          "lessThan",
          "greaterThan",
          "lessThanOrEquals",
          "greaterThanOrEquals"],

-- // 19.4 <exists predicate>
--
-- existsPredicate
--     : EXISTS (LEFT_BRACE graphPattern RIGHT_BRACE | LEFT_PAREN graphPattern RIGHT_PAREN | LEFT_BRACE matchStatementBlock RIGHT_BRACE | LEFT_PAREN matchStatementBlock RIGHT_PAREN | nestedQuerySpecification)
--     ;
      def "ExistsPredicate" $
        union [
          "graphPatternBrace">: gql "GraphPattern",
          "graphPatternParen">: gql "GraphPattern",
          "matchBlockBrace">: gql "MatchStatementBlock",
          "matchBlockParen">: gql "MatchStatementBlock",
          "nestedQuery">: gql "NestedQuerySpecification"],

-- // 19.5 <null predicate>
--
-- nullPredicate
--     : valueExpressionPrimary nullPredicatePart2
--     ;
      def "NullPredicate" $
        record [
          "valueExpression">: gql "PrimaryValueExpression",
          "nullPart">: gql "NullPredicatePart2"],

-- nullPredicatePart2
--     : IS NOT? NULL_KW
--     ;
      def "NullPredicatePart2" $
        record [
          "not">: boolean],

-- // 19.6 <value type predicate>
--
-- valueTypePredicate
--     : valueExpressionPrimary valueTypePredicatePart2
--     ;
      def "ValueTypePredicate" $
        record [
          "valueExpression">: gql "PrimaryValueExpression",
          "valueTypePart">: gql "ValueTypePredicatePart2"],

-- valueTypePredicatePart2
--     : IS NOT? typed valueType
--     ;
      def "ValueTypePredicatePart2" $
        record [
          "not">: boolean,
          "typed">: gql "Typed",
          "valueType">: gql "ValueType"],

-- // 19.7 <normalized predicate>
--
-- normalizedPredicatePart2
--     : IS NOT? normalForm? NORMALIZED
--     ;
      def "NormalizedPredicatePart2" $
        record [
          "not">: boolean,
          "normalForm">: optional $ gql "NormalForm"],

-- // 19.8 <directed predicate>
--
-- directedPredicate
--     : elementVariableReference directedPredicatePart2
--     ;
      def "DirectedPredicate" $
        record [
          "elementVariableReference">: gql "ElementVariableReference",
          "directedPart">: gql "DirectedPredicatePart2"],

-- directedPredicatePart2
--     : IS NOT? DIRECTED
--     ;
      def "DirectedPredicatePart2" $
        record [
          "not">: boolean],

-- // 19.9 <labeled predicate>
--
-- labeledPredicate
--     : elementVariableReference labeledPredicatePart2
--     ;
      def "LabeledPredicate" $
        record [
          "elementVariableReference">: gql "ElementVariableReference",
          "labeledPart">: gql "LabeledPredicatePart2"],

-- labeledPredicatePart2
--     : isLabeledOrColon labelExpression
--     ;
      def "LabeledPredicatePart2" $
        record [
          "isLabeledOrColon">: gql "IsLabeledOrColon",
          "labelExpression">: gql "LabelExpression"],

-- isLabeledOrColon
--     : IS NOT? LABELED
--     | COLON
--     ;
      def "IsLabeledOrColon" $
        union [
          "not">: boolean,
          "colon">: unit],

-- // 19.10 <source/destination predicate>
--
-- sourceDestinationPredicate
--     : nodeReference sourcePredicatePart2
--     | nodeReference destinationPredicatePart2
--     ;
      def "SourceDestinationPredicate" $
        union [
          "sourcePredicate">: gql "SourcePredicate",
          "destinationPredicate">: gql "DestinationPredicate"],

-- nodeReference
--     : elementVariableReference
--     ;
      def "NodeReference" $
        gql "ElementVariableReference",

-- sourcePredicatePart2
--     : IS NOT? SOURCE OF edgeReference
--     ;
      def "SourcePredicate" $
        record [
          "not">: boolean,
          "sourceOf">: gql "EdgeReference"],

-- destinationPredicatePart2
--     : IS NOT? DESTINATION OF edgeReference
--     ;
      def "DestinationPredicate" $
        record [
          "nodeReference">: gql "NodeReference",
          "not">: boolean,
          "destinationOf">: gql "EdgeReference"],

-- edgeReference
--     : elementVariableReference
--     ;
      def "EdgeReference" $
        gql "ElementVariableReference",

-- // 19.11 <all different predicate>
--
-- all_differentPredicate
--     : ALL_DIFFERENT LEFT_PAREN elementVariableReference COMMA elementVariableReference (COMMA elementVariableReference)* RIGHT_PAREN
--     ;
      def "AllDifferentPredicate" $
        record [
          "references">: nonemptyList $ gql "ElementVariableReference"],

-- // 19.12 <same predicate>
--
-- samePredicate
--     : SAME LEFT_PAREN elementVariableReference COMMA elementVariableReference (COMMA elementVariableReference)* RIGHT_PAREN
--     ;
      def "SamePredicate" $
        record [
          "references">: nonemptyList $ gql "ElementVariableReference"],

-- // 19.13 <property exists predicate>
--
-- property_existsPredicate
--     : PROPERTY_EXISTS LEFT_PAREN elementVariableReference COMMA propertyName RIGHT_PAREN
--     ;
      def "PropertyExistsPredicate" $
        record [
          "elementVariableReference">: gql "ElementVariableReference",
          "propertyName">: gql "PropertyName"],

-- // 20.1 <value expression>
--
-- valueExpression
--     : sign = (PLUS_SIGN | MINUS_SIGN) valueExpression                       #signedExprAlt
--     | valueExpression operator = (ASTERISK | SOLIDUS) valueExpression       #multDivExprAlt
--     | valueExpression operator = (PLUS_SIGN | MINUS_SIGN) valueExpression   #addSubtractExprAlt
--     | valueExpression CONCATENATION_OPERATOR valueExpression                #concatenationExprAlt
--     | NOT valueExpression                                                   #notExprAlt
--     | valueExpression IS NOT? truthValue                                    #isNotExprAlt
--     | valueExpression AND valueExpression                                   #conjunctiveExprAlt
--     | valueExpression operator = (OR | XOR) valueExpression                 #disjunctiveExprAlt
--     | valueExpression comparisonPredicatePart2                              #comparisonExprAlt
--     | predicate                                                             #predicateExprAlt
--     | valueExpression normalizedPredicatePart2                              #normalizedPredicateExprAlt
--     | PROPERTY? GRAPH graphExpression                                       #propertyGraphExprAlt
--     | BINDING? TABLE bindingTableExpression                                 #bindingTableExprAlt
--     | valueFunction                                                         #valueFunctionExprAlt
--     | valueExpressionPrimary                                                #primaryExprAlt
--     ;
      def "ValueExpression" $
        union [
          "signed">: gql "SignedExpr",
          "multDiv">: gql "MultDivExpr",
          "addSubtract">: gql "AddSubtractExpr",
          "concatenation">: gql "ConcatenationExpr",
          "not">: gql "NotExpr",
          "isNot">: gql "IsNotExpr",
          "conjunctive">: gql "ConjunctiveExpr",
          "disjunctive">: gql "DisjunctiveExpr",
          "comparison">: gql "ComparisonExpr",
          "predicate">: gql "Predicate",
          "normalizedPredicate">: gql "NormalizedPredicateExpr",
          "propertyGraph">: gql "GraphExpression",
          "bindingTable">: gql "BindingTableExpression",
          "valueFunction">: gql "ValueFunction",
          "primary">: gql "PrimaryValueExpression"],

      def "SignedExpr" $
        record [
          "sign">: gql "Sign",
          "valueExpression">: gql "ValueExpression"],

      def "MultDivExpr" $
        record [
          "left">: gql "ValueExpression",
          "operator">: gql "MultDivOperator",
          "right">: gql "ValueExpression"],

      def "AddSubtractExpr" $
        record [
          "left">: gql "ValueExpression",
          "operator">: gql "AddSubtractOperator",
          "right">: gql "ValueExpression"],

      def "ConcatenationExpr" $
        record [
          "left">: gql "ValueExpression",
          "right">: gql "ValueExpression"],

      def "NotExpr" $
        gql "ValueExpression",

      def "IsNotExpr" $
        record [
          "valueExpression">: gql "ValueExpression",
          "not">: boolean,
          "truthValue">: gql "TruthValue"],

      def "ConjunctiveExpr" $
        record [
          "left">: gql "ValueExpression",
          "right">: gql "ValueExpression"],

      def "DisjunctiveExpr" $
        record [
          "left">: gql "ValueExpression",
          "operator">: gql "DisjunctiveOperator",
          "right">: gql "ValueExpression"],

      def "ComparisonExpr" $
        record [
          "valueExpression">: gql "ValueExpression",
          "comparison">: gql "ComparisonPredicatePart2"],

      def "NormalizedPredicateExpr" $
        record [
          "valueExpression">: gql "ValueExpression",
          "normalizedPredicate">: gql "NormalizedPredicatePart2"],

      def "Sign" $
        enum ["plus", "minus"],

      def "MultDivOperator" $
        enum ["multiply", "divide"],

      def "AddSubtractOperator" $
        enum ["add", "subtract"],

      def "DisjunctiveOperator" $
        enum ["or", "xor"],

-- valueFunction
--     : numericValueFunction
--     | datetimeSubtraction
--     | datetimeValueFunction
--     | durationValueFunction
--     | characterOrByteStringFunction
--     | listValueFunction
--     ;
      def "ValueFunction" $
        union [
          "numeric">: gql "NumericValueFunction",
          "datetimeSubtraction">: gql "DatetimeSubtraction",
          "datetime">: gql "DatetimeValueFunction",
          "duration">: gql "DurationValueFunction",
          "characterOrByteString">: gql "CharacterOrByteStringFunction",
          "list">: gql "ListValueFunction"],

-- booleanValueExpression
--     : valueExpression
--     ;
      def "BooleanValueExpression" $
        gql "ValueExpression",

-- characterOrByteStringFunction
--     : subCharacterOrByteString
--     | trimSingleCharacterOrByteString
--     | foldCharacterString
--     | trimMultiCharacterCharacterString
--     | normalizeCharacterString
--     ;
      def "CharacterOrByteStringFunction" $
        union [
          "sub">: gql "SubCharacterOrByteString",
          "trimSingle">: gql "TrimSingleCharacterOrByteString",
          "fold">: gql "FoldCharacterString",
          "trimMultiCharacter">: gql "TrimMultiCharacterCharacterString",
          "normalize">: gql "NormalizeCharacterString"],

-- subCharacterOrByteString
--     : (LEFT | RIGHT) LEFT_PAREN valueExpression COMMA stringLength RIGHT_PAREN
--     ;
      def "SubCharacterOrByteString" $
        record [
          "side">: gql "Side",
          "valueExpression">: gql "ValueExpression",
          "stringLength">: gql "StringLength"],

      def "Side" $
        enum ["left", "right"],

-- trimSingleCharacterOrByteString
--     : TRIM LEFT_PAREN trimOperands RIGHT_PAREN
--     ;
      def "TrimSingleCharacterOrByteString" $
        gql "TrimOperands",

-- foldCharacterString
--     : (UPPER | LOWER) LEFT_PAREN valueExpression RIGHT_PAREN
--     ;
      def "FoldCharacterString" $
        record [
          "case">: gql "Case",
          "valueExpression">: gql "ValueExpression"],

      def "Case" $
        enum ["upper", "lower"],

-- trimMultiCharacterCharacterString
--     : (BTRIM | LTRIM | RTRIM) LEFT_PAREN valueExpression (COMMA valueExpression)? RIGHT_PAREN
--     ;
      def "TrimMultiCharacterCharacterString" $
        record [
          "trimType">: gql "TrimType",
          "valueExpression">: gql "ValueExpression",
          "optionalValueExpression">: optional $ gql "ValueExpression"],

      def "TrimType" $
        enum ["btrim", "ltrim", "rtrim"],

-- normalizeCharacterString
--     : NORMALIZE LEFT_PAREN valueExpression (COMMA normalForm)? RIGHT_PAREN
--     ;
      def "NormalizeCharacterString" $
        record [
          "valueExpression">: gql "ValueExpression",
          "normalForm">: optional $ gql "NormalForm"],

-- nodeReferenceValueExpression
--     : valueExpressionPrimary
--     ;
      def "NodeReferenceValueExpression" $
        gql "PrimaryValueExpression",

-- edgeReferenceValueExpression
--     : valueExpressionPrimary
--     ;
      def "EdgeReferenceValueExpression" $
        gql "PrimaryValueExpression",

-- aggregatingValueExpression
--     : valueExpression
--     ;
      def "AggregatingValueExpression" $
        gql "ValueExpression",

-- // 20.2 <value expression primary>
--
-- valueExpressionPrimary
--     : parenthesizedValueExpression
--     | aggregateFunction
--     | unsignedValueSpecification
-- // List and Record literals are redundantly/ambiguously part of the literal production
-- //    | listValueConstructor
-- //    | recordConstructor
--     | pathValueConstructor
--     | valueExpressionPrimary PERIOD propertyName      // <propertyReference
--     | valueQueryExpression
--     | caseExpression
--     | castSpecification
--     | element_idFunction
--     | letValueExpression
--     | bindingVariableReference
--     ;
      def "PrimaryValueExpression" $
        union [
          "parenthesized">: gql "ParenthesizedValueExpression",
          "aggregateFunction">: gql "AggregateFunction",
          "unsignedValueSpecification">: gql "UnsignedValueSpecification",
          "pathValueConstructor">: gql "PathValueConstructor",
          "propertyReference">: gql "PropertyReference",
          "valueQueryExpression">: gql "ValueQueryExpression",
          "caseExpression">: gql "CaseExpression",
          "castSpecification">: gql "CastSpecification",
          "elementIdFunction">: gql "ElementIdFunction",
          "letValueExpression">: gql "LetValueExpression",
          "bindingVariableReference">: gql "BindingVariableReference"],

-- parenthesizedValueExpression
--     : LEFT_PAREN valueExpression RIGHT_PAREN
--     ;
      def "ParenthesizedValueExpression" $
        gql "ValueExpression",

-- nonParenthesizedPrimaryValueExpression
--     : nonParenthesizedPrimaryValueExpressionSpecialCase
--     | bindingVariableReference
--     ;
      def "NonParenthesizedPrimaryValueExpression" $
        union [
          "special">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase",
          "bindingVariable">: gql "BindingVariableReference"],

-- nonParenthesizedPrimaryValueExpressionSpecialCase
--     : aggregateFunction
--     | unsignedValueSpecification
-- // List and Record literals are redundantly/ambiguously part of the literal production
-- //    | listValueConstructor
-- //    | recordConstructor
--     | pathValueConstructor
--     | valueExpressionPrimary PERIOD propertyName      // <property reference>
--     | valueQueryExpression
--     | caseExpression
--     | castSpecification
--     | element_idFunction
--     | letValueExpression
--     ;
      def "NonParenthesizedPrimaryValueExpressionSpecialCase" $
        union [
          "aggregateFunction">: gql "AggregateFunction",
          "unsignedValueSpecification">: gql "UnsignedValueSpecification",
          "pathValueConstructor">: gql "PathValueConstructor",
          "propertyReference">: gql "PropertyReference",
          "valueQueryExpression">: gql "ValueQueryExpression",
          "caseExpression">: gql "CaseExpression",
          "castSpecification">: gql "CastSpecification",
          "elementIdFunction">: gql "ElementIdFunction",
          "letValueExpression">: gql "LetValueExpression"],

-- // 20.3 <value specification>
--
-- unsignedValueSpecification
--     : unsignedLiteral
--     | generalValueSpecification
--     ;
      def "UnsignedValueSpecification" $
        union [
          "unsignedLiteral">: gql "UnsignedLiteral",
          "generalValueSpecification">: gql "GeneralValueSpecification"],

-- nonNegativeIntegerSpecification
--     : unsignedInteger
--     | dynamicParameterSpecification
--     ;
      def "NonNegativeIntegerSpecification" $
        union [
          "unsignedInteger">: gql "UnsignedInteger",
          "dynamicParameterSpecification">: gql "DynamicParameterSpecification"],

-- generalValueSpecification
--     : dynamicParameterSpecification
--     | SESSION_USER
--     ;
      def "GeneralValueSpecification" $
        union [
          "dynamicParameterSpecification">: gql "DynamicParameterSpecification",
          "sessionUser">: unit],

-- // 20.4 <dynamic parameter specification>
--
-- dynamicParameterSpecification
--     : GENERAL_PARAMETER_REFERENCE
--     ;
      def "DynamicParameterSpecification" $
        gql "ParameterName",

-- // 20.5 <let value expression>
--
-- letValueExpression
--     : LET letVariableDefinitionList IN valueExpression END
--     ;
      def "LetValueExpression" $
        record [
          "letVariables">: gql "LetVariableDefinitionList",
          "valueExpression">: gql "ValueExpression"],

-- // 20.6 <value query expression>
--
-- valueQueryExpression
--     : VALUE nestedQuerySpecification
--     ;
      def "ValueQueryExpression" $
        gql "NestedQuerySpecification",

-- // 20.7 <case expression>
--
-- caseExpression
--     : caseAbbreviation
--     | caseSpecification
--     ;
      def "CaseExpression" $
        union [
          "abbreviation">: gql "CaseAbbreviation",
          "specification">: gql "CaseSpecification"],

-- caseAbbreviation
--     : NULLIF LEFT_PAREN valueExpression COMMA valueExpression RIGHT_PAREN
--     | COALESCE LEFT_PAREN valueExpression (COMMA valueExpression)+ RIGHT_PAREN
--     ;
      def "CaseAbbreviation" $
        union [
          "nullIf">: gql "NullIfAbbreviation",
          "coalesce">: nonemptyList $ gql "ValueExpression"],

      def "NullIfAbbreviation" $
        record [
          "first">: gql "ValueExpression",
          "second">: gql "ValueExpression"],

-- caseSpecification
--     : simpleCase
--     | searchedCase
--     ;
      def "CaseSpecification" $
        union [
          "simple">: gql "SimpleCase",
          "searched">: gql "SearchedCase"],

-- simpleCase
--     : CASE caseOperand simpleWhenClause+ elseClause? END
--     ;
      def "SimpleCase" $
        record [
          "caseOperand">: gql "CaseOperand",
          "whenClauses">: nonemptyList $ gql "SimpleWhenClause",
          "elseClause">: optional $ gql "ElseClause"],

-- searchedCase
--     : CASE searchedWhenClause+ elseClause? END
--     ;
      def "SearchedCase" $
        record [
          "whenClauses">: nonemptyList $ gql "SearchedWhenClause",
          "elseClause">: optional $ gql "ElseClause"],

-- simpleWhenClause
--     : WHEN whenOperandList THEN result
--     ;
      def "SimpleWhenClause" $
        record [
          "whenOperands">: gql "WhenOperandList",
          "result">: gql "Result"],

-- searchedWhenClause
--     : WHEN searchCondition THEN result
--     ;
      def "SearchedWhenClause" $
        record [
          "searchCondition">: gql "SearchCondition",
          "result">: gql "Result"],

-- elseClause
--     : ELSE result
--     ;
      def "ElseClause" $
        gql "Result",

-- caseOperand
--     : nonParenthesizedPrimaryValueExpression
--     | elementVariableReference
--     ;
      def "CaseOperand" $
        union [
          "valueExpression">: gql "NonParenthesizedPrimaryValueExpression",
          "elementReference">: gql "ElementVariableReference"],

-- whenOperandList
--     : whenOperand (COMMA whenOperand)*
--     ;
      def "WhenOperandList" $
        nonemptyList $ gql "WhenOperand",

-- whenOperand
--     : nonParenthesizedPrimaryValueExpression
--     | comparisonPredicatePart2
--     | nullPredicatePart2
--     | valueTypePredicatePart2
--     | normalizedPredicatePart2
--     | directedPredicatePart2
--     | labeledPredicatePart2
--     | sourcePredicatePart2
--     | destinationPredicatePart2
--     ;
      def "WhenOperand" $
        union [
          "valueExpression">: gql "NonParenthesizedPrimaryValueExpression",
          "comparison">: gql "ComparisonPredicatePart2",
          "nullPredicate">: gql "NullPredicatePart2",
          "valueTypePredicate">: gql "ValueTypePredicatePart2",
          "normalizedPredicate">: gql "NormalizedPredicatePart2",
          "directedPredicate">: gql "DirectedPredicatePart2",
          "labeledPredicate">: gql "LabeledPredicatePart2",
          "sourcePredicate">: gql "SourcePredicate",
          "destinationPredicate">: gql "DestinationPredicate"],

-- result
--     : resultExpression
--     | nullLiteral
--     ;
      def "Result" $
        union [
          "simple">: gql "ResultExpression",
          "nullLiteral">: unit],

-- resultExpression
--     : valueExpression
--     ;
      def "ResultExpression" $
        gql "ValueExpression",

-- // 20.8 <cast specification>
--
-- castSpecification
--     : CAST LEFT_PAREN castOperand AS castTarget RIGHT_PAREN
--     ;
      def "CastSpecification" $
        record [
          "operand">: gql "CastOperand",
          "target">: gql "CastTarget"],

-- castOperand
--     : valueExpression
--     | nullLiteral
--     ;
      def "CastOperand" $
        union [
          "valueExpression">: gql "ValueExpression",
          "nullLiteral">: unit],

-- castTarget
--     : valueType
--     ;
      def "CastTarget" $
        gql "ValueType",

-- // 20.9 <aggregate function>
--
-- aggregateFunction
--     : COUNT LEFT_PAREN ASTERISK RIGHT_PAREN
--     | generalSetFunction
--     | binarySetFunction
--     ;
      def "AggregateFunction" $
        union [
          "countAll">: unit,
          "generalSetFunction">: gql "GeneralSetFunction",
          "binarySetFunction">: gql "BinarySetFunction"],

-- generalSetFunction
--     : generalSetFunctionType LEFT_PAREN setQuantifier? valueExpression RIGHT_PAREN
--     ;
      def "GeneralSetFunction" $
        record [
          "functionType">: gql "GeneralSetFunctionType",
          "setQuantifier">: optional $ gql "SetQuantifier",
          "valueExpression">: gql "ValueExpression"],

-- binarySetFunction
--     : binarySetFunctionType LEFT_PAREN dependentValueExpression COMMA independentValueExpression RIGHT_PAREN
--     ;
      def "BinarySetFunction" $
        record [
          "functionType">: gql "BinarySetFunctionType",
          "dependentValue">: gql "DependentValueExpression",
          "independentValue">: gql "IndependentValueExpression"],

-- generalSetFunctionType
--     : AVG
--     | COUNT
--     | MAX
--     | MIN
--     | SUM
--     | COLLECT_LIST
--     | STDDEV_SAMP
--     | STDDEV_POP
--     ;
      def "GeneralSetFunctionType" $
        enum [
          "avg",
          "count",
          "max",
          "min",
          "sum",
          "collectList",
          "stddevSamp",
          "stddevPop"],

-- setQuantifier
--     : DISTINCT
--     | ALL
--     ;
      def "SetQuantifier" $
        enum ["distinct", "all"],

-- binarySetFunctionType
--     : PERCENTILE_CONT
--     | PERCENTILE_DISC
--     ;
      def "BinarySetFunctionType" $
        enum ["percentileCont", "percentileDisc"],

-- dependentValueExpression
--     : setQuantifier? numericValueExpression
--     ;
      def "DependentValueExpression" $
        record [
          "setQuantifier">: optional $ gql "SetQuantifier",
          "numericValue">: gql "NumericValueExpression"],

-- independentValueExpression
--     : numericValueExpression
--     ;
      def "IndependentValueExpression" $
        gql "NumericValueExpression",

-- // 20.10 <element_id function>
--
-- element_idFunction
--     : ELEMENT_ID LEFT_PAREN elementVariableReference RIGHT_PAREN
--     ;
      def "ElementIdFunction" $
        gql "ElementVariableReference",

-- // 20.11 <property reference>
--
-- propertyReference
--     : valueExpressionPrimary PERIOD propertyName
--     ;
      def "PropertyReference" $
        record [
          "valueExpression">: gql "PrimaryValueExpression",
          "propertyName">: gql "PropertyName"],

-- // 20.12 <binding variable reference>
--
-- bindingVariableReference
--     : bindingVariable
--     ;
      def "BindingVariableReference" $
        gql "BindingVariable",

-- pathValueExpression
--     : valueExpression
--     ;
      def "PathValueExpression" $
        gql "ValueExpression",

-- // 20.14 <path value constructor>
--
-- pathValueConstructor
--     : pathValueConstructorByEnumeration
--     ;
      def "PathValueConstructor" $
        gql "PathValueConstructorByEnumeration",

-- pathValueConstructorByEnumeration
--     : PATH LEFT_BRACKET pathElementList RIGHT_BRACKET
--     ;
      def "PathValueConstructorByEnumeration" $
        gql "PathElementList",

-- pathElementList
--     : pathElementListStart pathElementListStep*
--     ;
      def "PathElementList" $
        record [
          "start">: gql "PathElementListStart",
          "steps">: list $ gql "PathElementListStep"],

-- pathElementListStart
--     : nodeReferenceValueExpression
--     ;
      def "PathElementListStart" $
        gql "NodeReferenceValueExpression",

-- pathElementListStep
--     : COMMA edgeReferenceValueExpression COMMA nodeReferenceValueExpression
--     ;
      def "PathElementListStep" $
        record [
          "edgeReference">: gql "EdgeReferenceValueExpression",
          "nodeReference">: gql "NodeReferenceValueExpression"],

-- // 20.15 <list value expression>
--
-- listValueExpression
--     : valueExpression
--     ;
      def "ListValueExpression" $
        gql "ValueExpression",

-- // 20.16 <list value function>
--
-- listValueFunction
--     : trimListFunction
--     | elementsFunction
--     ;
      def "ListValueFunction" $
        union [
          "trim">: gql "TrimListFunction",
          "elements">: gql "ElementsFunction"],

-- trimListFunction
--     : TRIM LEFT_PAREN listValueExpression COMMA numericValueExpression RIGHT_PAREN
--     ;
      def "TrimListFunction" $
        record [
          "listValue">: gql "ListValueExpression",
          "numericValue">: gql "NumericValueExpression"],

-- elementsFunction
--     : ELEMENTS LEFT_PAREN pathValueExpression RIGHT_PAREN
--     ;
      def "ElementsFunction" $
        gql "PathValueExpression",

-- // 20.17 <list value constructor>
--
-- listValueConstructor
--     : listValueConstructorByEnumeration
--     ;
      def "ListValueConstructor" $
        gql "ListValueConstructorByEnumeration",

-- listValueConstructorByEnumeration
--     : listValueTypeName? LEFT_BRACKET listElementList? RIGHT_BRACKET
--     ;
      def "ListValueConstructorByEnumeration" $
        record [
          "listValueTypeName">: optional $ gql "ListValueTypeName",
          "elements">: optional $ gql "ListElementList"],

-- listElementList
--     : listElement (COMMA listElement)*
--     ;
      def "ListElementList" $
        nonemptyList $ gql "ListElement",

-- listElement
--     : valueExpression
--     ;
      def "ListElement" $
        gql "ValueExpression",

-- // 20.18 <record constructor>
--
-- recordConstructor
--     : RECORD? fieldsSpecification
--     ;
      def "RecordConstructor" $
        gql "FieldsSpecification",

-- fieldsSpecification
--     : LEFT_BRACE fieldList? RIGHT_BRACE
--     ;
      def "FieldsSpecification" $
        optional $ gql "FieldList",

-- fieldList
--     : field (COMMA field)*
--     ;
      def "FieldList" $
        nonemptyList $ gql "Field",

-- // 20.19 <field>
--
-- field
--     : fieldName COLON valueExpression
--     ;
      def "Field" $
        record [
          "name">: gql "FieldName",
          "value">: gql "ValueExpression"],

-- // 20.20 <boolean value expression>
--
-- truthValue
--     : BOOLEAN_LITERAL
--     ;
      def "TruthValue" $
        gql "BooleanLiteral",

-- // 20.21 <numeric value expression>
--
-- numericValueExpression
--     : sign = (PLUS_SIGN | MINUS_SIGN) numericValueExpression
--     | numericValueExpression operator = (ASTERISK | SOLIDUS) numericValueExpression
--     | numericValueExpression operator = (PLUS_SIGN | MINUS_SIGN) numericValueExpression
--     | valueExpressionPrimary
--     | numericValueFunction
--     ;
      def "NumericValueExpression" $
        union [
          "signed">: gql "SignedNumericValueExpression",
          "multiplicationOrDivision">: gql "MulDivNumericValueExpression",
          "additionOrSubtraction">: gql "AddSubNumericValueExpression",
          "primary">: gql "PrimaryValueExpression",
          "function">: gql "NumericValueFunction"],

      def "SignedNumericValueExpression" $
        record [
          "sign">: gql "Sign",
          "expression">: gql "NumericValueExpression"],

      def "MulDivNumericValueExpression" $
        record [
          "left">: gql "NumericValueExpression",
          "operator">: gql "MultDivOperator",
          "right">: gql "NumericValueExpression"],

      def "AddSubNumericValueExpression" $
        record [
          "left">: gql "NumericValueExpression",
          "operator">: gql "AddSubtractOperator",
          "right">: gql "NumericValueExpression"],

-- // 20.22 <numeric value function>
--
-- numericValueFunction
--     : lengthExpression
--     | cardinalityExpression
--     | absoluteValueExpression
--     | modulusExpression
--     | trigonometricFunction
--     | generalLogarithmFunction
--     | commonLogarithm
--     | naturalLogarithm
--     | exponentialFunction
--     | powerFunction
--     | squareRoot
--     | floorFunction
--     | ceilingFunction
--     ;
      def "NumericValueFunction" $
        union [
          "length">: gql "LengthExpression",
          "cardinality">: gql "CardinalityExpression",
          "absoluteValue">: gql "AbsoluteValueExpression",
          "modulus">: gql "ModulusExpression",
          "trigonometric">: gql "TrigonometricFunction",
          "logarithm">: gql "GeneralLogarithmFunction",
          "commonLogarithm">: gql "CommonLogarithm",
          "naturalLogarithm">: gql "NaturalLogarithm",
          "exponential">: gql "ExponentialFunction",
          "power">: gql "PowerFunction",
          "squareRoot">: gql "SquareRoot",
          "floor">: gql "FloorFunction",
          "ceiling">: gql "CeilingFunction"],

-- lengthExpression
--     : charLengthExpression
--     | byteLengthExpression
--     | pathLengthExpression
--     ;
      def "LengthExpression" $
        union [
          "char">: gql "CharLengthExpression",
          "byte">: gql "ByteLengthExpression",
          "path">: gql "PathLengthExpression"],

-- cardinalityExpression
--     : CARDINALITY LEFT_PAREN cardinalityExpressionArgument RIGHT_PAREN
--     | SIZE LEFT_PAREN listValueExpression RIGHT_PAREN
--     ;
      def "CardinalityExpression" $
        union [
          "cardinality">: gql "CardinalityArgumentExpression",
          "size">: gql "ListValueExpression"],

      def "CardinalityArgumentExpression" $
        gql "ValueExpression",

-- charLengthExpression
--     : (CHAR_LENGTH | CHARACTER_LENGTH) LEFT_PAREN characterStringValueExpression RIGHT_PAREN
--     ;
      def "CharLengthExpression" $
        gql "CharacterStringValueExpression",

-- byteLengthExpression
--     : (BYTE_LENGTH | OCTET_LENGTH) LEFT_PAREN byteStringValueExpression RIGHT_PAREN
--     ;
      def "ByteLengthExpression" $
        gql "ByteStringValueExpression",

-- pathLengthExpression
--     : PATH_LENGTH LEFT_PAREN pathValueExpression RIGHT_PAREN
--     ;
      def "PathLengthExpression" $
        gql "PathValueExpression",

-- absoluteValueExpression
--     : ABS LEFT_PAREN valueExpression RIGHT_PAREN
--     ;
      def "AbsoluteValueExpression" $
        gql "ValueExpression",

-- modulusExpression
--     : MOD LEFT_PAREN numericValueExpressionDividend COMMA numericValueExpressionDivisor RIGHT_PAREN
--     ;
      def "ModulusExpression" $
        record [
          "dividend">: gql "NumericValueExpressionDividend",
          "divisor">: gql "NumericValueExpressionDivisor"],

-- numericValueExpressionDividend
--     : numericValueExpression
--     ;
      def "NumericValueExpressionDividend" $
        gql "NumericValueExpression",

-- numericValueExpressionDivisor
--     : numericValueExpression
--     ;
      def "NumericValueExpressionDivisor" $
        gql "NumericValueExpression",

-- trigonometricFunction
--     : trigonometricFunctionName LEFT_PAREN numericValueExpression RIGHT_PAREN
--     ;
      def "TrigonometricFunction" $
        record [
          "name">: gql "TrigonometricFunctionName",
          "value">: gql "NumericValueExpression"],

-- trigonometricFunctionName
--     : SIN
--     | COS
--     | TAN
--     | COT
--     | SINH
--     | COSH
--     | TANH
--     | ASIN
--     | ACOS
--     | ATAN
--     | DEGREES
--     | RADIANS
--     ;
      def "TrigonometricFunctionName" $
        enum ["sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "asin", "acos", "atan", "degrees", "radians"],

-- generalLogarithmFunction
--     : LOG LEFT_PAREN generalLogarithmBase COMMA generalLogarithmArgument RIGHT_PAREN
--     ;
      def "GeneralLogarithmFunction" $
        record [
          "base">: gql "GeneralLogarithmBase",
          "argument">: gql "GeneralLogarithmArgument"],

-- generalLogarithmBase
--     : numericValueExpression
--     ;
      def "GeneralLogarithmBase" $
        gql "NumericValueExpression",

-- generalLogarithmArgument
--     : numericValueExpression
--     ;
      def "GeneralLogarithmArgument" $
        gql "NumericValueExpression",

-- commonLogarithm
--     : LOG10 LEFT_PAREN numericValueExpression RIGHT_PAREN
--     ;
      def "CommonLogarithm" $
        gql "NumericValueExpression",

-- naturalLogarithm
--     : LN LEFT_PAREN numericValueExpression RIGHT_PAREN
--     ;
      def "NaturalLogarithm" $
        gql "NumericValueExpression",

-- exponentialFunction
--     : EXP LEFT_PAREN numericValueExpression RIGHT_PAREN
--     ;
      def "ExponentialFunction" $
        gql "NumericValueExpression",

-- powerFunction
--     : POWER LEFT_PAREN numericValueExpressionBase COMMA numericValueExpressionExponent RIGHT_PAREN
--     ;
      def "PowerFunction" $
        record [
          "base">: gql "NumericValueExpressionBase",
          "exponent">: gql "NumericValueExpressionExponent"],

-- numericValueExpressionBase
--     : numericValueExpression
--     ;
      def "NumericValueExpressionBase" $
        gql "NumericValueExpression",

-- numericValueExpressionExponent
--     : numericValueExpression
--     ;
      def "NumericValueExpressionExponent" $
        gql "NumericValueExpression",

-- squareRoot
--     : SQRT LEFT_PAREN numericValueExpression RIGHT_PAREN
--     ;
      def "SquareRoot" $
        gql "NumericValueExpression",

-- floorFunction
--     : FLOOR LEFT_PAREN numericValueExpression RIGHT_PAREN
--     ;
      def "FloorFunction" $
        gql "NumericValueExpression",

-- ceilingFunction
--     : (CEIL | CEILING) LEFT_PAREN numericValueExpression RIGHT_PAREN
--     ;
      def "CeilingFunction" $
        gql "NumericValueExpression",

-- // 20.23 <string value expression>
--
-- characterStringValueExpression
--     : valueExpression
--     ;
      def "CharacterStringValueExpression" $
        gql "ValueExpression",

-- byteStringValueExpression
--     : valueExpression
--     ;
      def "ByteStringValueExpression" $
        gql "ValueExpression",

-- // 20.24 <string value function>
--
-- trimOperands
--     : (trimSpecification? trimCharacterOrByteString? FROM)? trimCharacterOrByteStringSource
--     ;
      def "TrimOperands" $
        record [
          "specification">: optional $ gql "TrimSpecification",
          "characterOrByteString">: optional $ gql "TrimCharacterOrByteString",
          "source">: gql "TrimCharacterOrByteStringSource"],

-- trimCharacterOrByteStringSource
--     : valueExpression
--     ;
      def "TrimCharacterOrByteStringSource" $
        gql "ValueExpression",

-- trimSpecification
--     : LEADING
--     | TRAILING
--     | BOTH
--     ;
      def "TrimSpecification" $
        enum ["leading", "trailing", "both"],

-- trimCharacterOrByteString
--     : valueExpression
--     ;
      def "TrimCharacterOrByteString" $
        gql "ValueExpression",

-- normalForm
--     : NFC
--     | NFD
--     | NFKC
--     | NFKD
--     ;
      def "NormalForm" $
        enum ["nfc", "nfd", "nfkc", "nfkd"],

-- stringLength
--     : numericValueExpression
--     ;
      def "StringLength" $
        gql "NumericValueExpression",

-- // 20.25 <byte string function>
--
-- // Note: ByteString functions were moved to characterByteStringOrListFunction, some alternatives
-- // apply to characterString, byteString and list. Breaking them out separately resulted in
-- // ambiguity.
--

-- // 20.26 <datetime value expression>
--
-- // The implementation should enforce that the data type is a datetime value.
-- datetimeValueExpression
--      : valueExpression
--      ;
      def "DatetimeValueExpression" $
        gql "ValueExpression",

-- // 20.27 <datetime value function>
--
-- datetimeValueFunction
--     : dateFunction
--     | timeFunction
--     | datetimeFunction
--     | localtimeFunction
--     | localdatetimeFunction
--     ;
      def "DatetimeValueFunction" $
        union [
          "dateFunction">: gql "DateFunction",
          "timeFunction">: gql "TimeFunction",
          "datetimeFunction">: gql "DatetimeFunction",
          "localtimeFunction">: gql "LocaltimeFunction",
          "localdatetimeFunction">: gql "LocaldatetimeFunction"],

-- dateFunction
--     : CURRENT_DATE
--     | DATE LEFT_PAREN dateFunctionParameters? RIGHT_PAREN
--     ;
      def "DateFunction" $
        union [
          "currentDate">: unit,
          "dateWithParams">: optional $ gql "DateFunctionParameters"],

-- timeFunction
--     : CURRENT_TIME
--     | ZONED_TIME LEFT_PAREN timeFunctionParameters? RIGHT_PAREN
--     ;
      def "TimeFunction" $
        union [
          "currentTime">: unit,
          "zonedTimeWithParams">: optional $ gql "TimeFunctionParameters"],

-- localtimeFunction
--     : LOCAL_TIME (LEFT_PAREN timeFunctionParameters? RIGHT_PAREN)?
--     ;
      def "LocaltimeFunction" $
        optional $ gql "TimeFunctionParameters",

-- datetimeFunction
--     : CURRENT_TIMESTAMP
--     | ZONED_DATETIME LEFT_PAREN datetimeFunctionParameters? RIGHT_PAREN
--     ;
      def "DatetimeFunction" $
        union [
          "currentTimestamp">: unit,
          "zonedDatetimeWithParams">: optional $ gql "DatetimeFunctionParameters"],

-- localdatetimeFunction
--     : LOCAL_TIMESTAMP
--     | LOCAL_DATETIME LEFT_PAREN datetimeFunctionParameters? RIGHT_PAREN
--     ;
      def "LocaldatetimeFunction" $
        union [
          "localTimestamp">: unit,
          "localDatetimeWithParams">: optional $ gql "DatetimeFunctionParameters"],

-- dateFunctionParameters
--     : dateString
--     | recordConstructor
--     ;
      def "DateFunctionParameters" $
        union [
          "dateString">: gql "DateString",
          "recordConstructor">: gql "RecordConstructor"],

-- timeFunctionParameters
--     : timeString
--     | recordConstructor
--     ;
      def "TimeFunctionParameters" $
        union [
          "timeString">: gql "TimeString",
          "recordConstructor">: gql "RecordConstructor"],

-- datetimeFunctionParameters
--     : datetimeString
--     | recordConstructor
--     ;
      def "DatetimeFunctionParameters" $
        union [
          "datetimeString">: gql "DatetimeString",
          "recordConstructor">: gql "RecordConstructor"],

-- // 20.28 <duration value expression>
--
-- // The implementation should enforce that the data type is a duration value.
-- durationValueExpression
--     : valueExpression
--     ;
      def "DurationValueExpression" $
        gql "ValueExpression",

-- datetimeSubtraction
--     : DURATION_BETWEEN LEFT_PAREN datetimeSubtractionParameters RIGHT_PAREN temporalDurationQualifier?
--     ;
      def "DatetimeSubtraction" $
        record [
          "parameters">: gql "DatetimeSubtractionParameters",
          "temporalDurationQualifier">: optional $ gql "TemporalDurationQualifier"],

-- datetimeSubtractionParameters
--     : datetimeValueExpression1 COMMA datetimeValueExpression2
--     ;
      def "DatetimeSubtractionParameters" $
        record [
          "expression1">: gql "DatetimeValueExpression1",
          "expression2">: gql "DatetimeValueExpression2"],

-- datetimeValueExpression1
--     : datetimeValueExpression
--     ;
      def "DatetimeValueExpression1" $
        gql "DatetimeValueExpression",

-- datetimeValueExpression2
--     : datetimeValueExpression
--     ;
      def "DatetimeValueExpression2" $
        gql "DatetimeValueExpression",

-- temporalDurationQualifier
--     : YEAR TO MONTH
--     | DAY TO SECOND
--     ;

-- // 20.29 <duration value function>
--
-- durationValueFunction
--     : durationFunction
--     | absoluteValueExpression
--     ;
      def "DurationValueFunction" $
        union [
          "durationFunction">: gql "DurationFunction",
          "absoluteValue">: gql "AbsoluteValueExpression"],

-- durationFunction
--     : DURATION LEFT_PAREN durationFunctionParameters RIGHT_PAREN
--     ;
      def "DurationFunction" $
        gql "DurationFunctionParameters",

-- durationFunctionParameters
--     : durationString
--     | recordConstructor
--     ;
      def "DurationFunctionParameters" $
        union [
          "durationString">: gql "DurationString",
          "recordConstructor">: gql "RecordConstructor"],

-- // 21.1 Names and Variables
--
-- objectName
--     : identifier
--     ;
      def "ObjectName"
        string,

-- objectNameOrBindingVariable
--     : regularIdentifier
--     ;
      def "ObjectNameOrBindingVariable"
        string,

-- directoryName
--     : identifier
--     ;
      def "DirectoryName"
        string,

-- schemaName
--     : identifier
--     ;
      def "SchemaName" $
        string,

-- graphName
--     : regularIdentifier
--     | delimitedGraphName
--     ;
      def "GraphName" $
        string,

-- delimitedGraphName
--     // DELIMITED_IDENTIFIER
--     : DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     | ACCENT_QUOTED_CHARACTER_SEQUENCE
--     ;
      def "DelimitedGraphName"
        string,

-- graphTypeName
--     : identifier
--     ;
      def "GraphTypeName"
        string,

-- nodeTypeName
--     : identifier
--     ;
      def "NodeTypeName"
        string,

-- edgeTypeName
--     : identifier
--     ;
      def "EdgeTypeName"
        string,

-- bindingTableName
--     : regularIdentifier
--     | delimitedBindingTableName
--     ;
      def "BindingTableName" $
        union [
          "regularIdentifier">: string,
          "delimitedBindingTableName">: gql "DelimitedBindingTableName"],

-- delimitedBindingTableName
--      // DELIMITED_IDENTIFIER
--      : DOUBLE_QUOTED_CHARACTER_SEQUENCE
--      | ACCENT_QUOTED_CHARACTER_SEQUENCE
--      ;
      def "DelimitedBindingTableName"
        string,

-- procedureName
--     : identifier
--     ;
      def "ProcedureName"
        string,

-- labelName
--     : identifier
--     ;
      def "LabelName"
        string,

-- propertyName
--     : identifier
--     ;
      def "PropertyName"
        string,

-- fieldName
--     : identifier
--     ;
      def "FieldName"
        string,

-- elementVariable
--     : bindingVariable
--     ;
      def "ElementVariable" $
        gql "BindingVariable",

-- pathVariable
--     : bindingVariable
--     ;
      def "PathVariable" $
        gql "BindingVariable",

-- subpathVariable
--     : regularIdentifier
--     ;
      def "SubpathVariable"
        string,

-- bindingVariable
--     : regularIdentifier
--     ;
      def "BindingVariable"
        string,

-- // 21.2 <literal>
--
-- unsignedLiteral
--     : unsignedNumericLiteral
--     | generalLiteral
--     ;
      def "UnsignedLiteral" $
        union [
          "numeric">: gql "UnsignedNumericLiteral",
          "general">: gql "GeneralLiteral"],

-- generalLiteral
--     : BOOLEAN_LITERAL
--     | characterStringLiteral
--     | BYTE_STRING_LITERAL
--     | temporalLiteral
--     | durationLiteral
--     | nullLiteral
--     | listLiteral
--     | recordLiteral
--     ;
      def "GeneralLiteral" $
        union [
          "boolean">: gql "BooleanLiteral",
          "characterString">: gql "CharacterStringLiteral",
          "byteString">: gql "ByteStringLiteral",
          "temporal">: gql "TemporalLiteral",
          "duration">: gql "DurationLiteral",
          "nullLiteral">: gql "NullLiteral",
          "list">: gql "ListLiteral",
          "record">: gql "RecordLiteral"],

-- temporalLiteral
--     : dateLiteral
--     | timeLiteral
--     | datetimeLiteral
--     ;
      def "TemporalLiteral" $
        union [
          "date">: gql "DateLiteral",
          "time">: gql "TimeLiteral",
          "datetime">: gql "DatetimeLiteral"],

-- dateLiteral
--     : DATE dateString
--     ;
      def "DateLiteral" $
        gql "DateString",

-- timeLiteral
--     : TIME timeString
--     ;
      def "TimeLiteral" $
        gql "TimeString",

-- datetimeLiteral
--     : (DATETIME | TIMESTAMP) datetimeString
--     ;
      def "DatetimeLiteral" $
        gql "DatetimeString",

-- listLiteral
--     : listValueConstructorByEnumeration
--     ;
      def "ListLiteral" $
        gql "ListValueConstructorByEnumeration",

-- recordLiteral
--     : recordConstructor
--     ;
      def "RecordLiteral" $
        gql "RecordConstructor",

-- identifier
--     : regularIdentifier
--     | DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     | ACCENT_QUOTED_CHARACTER_SEQUENCE
--     ;
      def "Identifier"
        string,

-- regularIdentifier
--     : REGULAR_IDENTIFIER
--     | nonReservedWords
--     ;
      def "RegularIdentifier" $
        string,

-- timeZoneString
--     : characterStringLiteral
--     ;
      def "TimeZoneString" $
        gql "CharacterStringLiteral",

-- characterStringLiteral
--     : SINGLE_QUOTED_CHARACTER_SEQUENCE
--     | DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     ;
      def "CharacterStringLiteral" $
        string,

-- unsignedNumericLiteral
--     : exactNumericLiteral
--     | approximateNumericLiteral
--     ;
      def "UnsignedNumericLiteral" $
        union [
          "exact">: gql "ExactNumericLiteral",
          "approximate">: gql "ApproximateNumericLiteral"],

-- exactNumericLiteral
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_EXACT_NUMBER_SUFFIX
--     | UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_EXACT_NUMBER_SUFFIX
--     | UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITHOUT_SUFFIX
--     | UNSIGNED_DECIMAL_INTEGER_WITH_EXACT_NUMBER_SUFFIX
--     | unsignedInteger
--     ;
      def "ExactNumericLiteral" $
        union [
          "scientificWithSuffix">: string,
          "commonWithSuffix">: string,
          "commonWithoutSuffix">: string,
          "integerWithSuffix">: string,
          "unsignedInteger">: gql "UnsignedInteger"],

-- approximateNumericLiteral
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX
--     | UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITHOUT_SUFFIX
--     | UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX
--     | UNSIGNED_DECIMAL_INTEGER_WITH_APPROXIMATE_NUMBER_SUFFIX
--     ;
      def "ApproximateNumericLiteral" $
        union [
          "scientificWithSuffix">: string,
          "scientificWithoutSuffix">: string,
          "commonWithSuffix">: string,
          "integerWithSuffix">: string],

-- unsignedInteger
--     : UNSIGNED_DECIMAL_INTEGER
--     | UNSIGNED_HEXADECIMAL_INTEGER
--     | UNSIGNED_OCTAL_INTEGER
--     | UNSIGNED_BINARY_INTEGER
--     ;
      def "UnsignedInteger" $
        union [
          "decimal">: string,
          "hexadecimal">: string,
          "octal">: string,
          "binary">: string],

-- unsignedDecimalInteger
--     : UNSIGNED_DECIMAL_INTEGER
--     ;
      def "UnsignedDecimalInteger"
        string,

-- nullLiteral
--     : NULL_KW
--     ;
      def "NullLiteral" $
        unit,

-- dateString
--     : characterStringLiteral
--     ;
      def "DateString" $
        gql "CharacterStringLiteral",

-- timeString
--     : characterStringLiteral
--     ;
      def "TimeString" $
        gql "CharacterStringLiteral",

-- datetimeString
--     : characterStringLiteral
--     ;
      def "DatetimeString" $
        gql "CharacterStringLiteral",

-- durationLiteral
--     : DURATION durationString
--     ;
      def "DurationLiteral" $
        gql "DurationString",

-- durationString
--     : characterStringLiteral
--     ;
      def "DurationString" $
        gql "CharacterStringLiteral",

-- nodeSynonym
--     : NODE
--     | VERTEX
--     ;
      def "NodeSynonym" $
        enum ["node", "vertex"],

-- edgesSynonym
--     : EDGES
--     | RELATIONSHIPS
--     ;
      def "EdgesSynonym" $
        enum ["edges", "relationships"],

-- edgeSynonym
--     : EDGE
--     | RELATIONSHIP
--     ;
      def "EdgeSynonym" $
        enum ["edge", "relationship"],

-- // 21.1 Names and Variables
--
-- IMPLIES
--     : RIGHT_DOUBLE_ARROW
--     | 'IMPLIES'
--     ;
      def "Implies" $
        enum ["rightDoubleArrow", "implies"],

-- fragment PARAMETER_NAME
--     : SEPARATED_IDENTIFIER
--     ;
      def "ParameterName"
        string,

-- // 21.2 <literal>
--
-- nonReservedWords
--     : ACYCLIC
--     | BINDING
--     | BINDINGS
--     | CONNECTING
--     | DESTINATION
--     | DIFFERENT
--     | DIRECTED
--     | EDGE
--     | EDGES
--     | ELEMENT
--     | ELEMENTS
--     | FIRST
--     | GRAPH
--     | GROUPS
--     | KEEP
--     | LABEL
--     | LABELED
--     | LABELS
--     | LAST
--     | NFC
--     | NFD
--     | NFKC
--     | NFKD
--     | NO
--     | NODE
--     | NORMALIZED
--     | ONLY
--     | ORDINALITY
--     | PROPERTY
--     | READ
--     | RELATIONSHIP
--     | RELATIONSHIPS
--     | REPEATABLE
--     | SHORTEST
--     | SIMPLE
--     | SOURCE
--     | TABLE
--     | TEMP
--     | TO
--     | TRAIL
--     | TRANSACTION
--     | TYPE
--     | UNDIRECTED
--     | VERTEX
--     | WALK
--     | WITHOUT
--     | WRITE
--     | ZONE
--     ;
-- BOOLEAN_LITERAL
--     : 'TRUE'
--     | 'FALSE'
--     | 'UNKNOWN'
--     ;
      def "BooleanLiteral" $
        enum ["true", "false", "unknown"],

-- SINGLE_QUOTED_CHARACTER_SEQUENCE
--     : NO_ESCAPE? UNBROKEN_SINGLE_QUOTED_CHARACTER_SEQUENCE
--     ;
--
-- DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     : NO_ESCAPE? UNBROKEN_DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     ;
--
-- ACCENT_QUOTED_CHARACTER_SEQUENCE
--     :NO_ESCAPE? UNBROKEN_ACCENT_QUOTED_CHARACTER_SEQUENCE
--     ;
--
-- NO_ESCAPE
--     : COMMERCIAL_AT
--     ;
--
-- fragment UNBROKEN_SINGLE_QUOTED_CHARACTER_SEQUENCE
--     : QUOTE SINGLE_QUOTED_CHARACTER_REPRESENTATION* QUOTE
--     ;
--
-- fragment UNBROKEN_DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     : DOUBLE_QUOTE DOUBLE_QUOTED_CHARACTER_REPRESENTATION* DOUBLE_QUOTE
--     ;
--
-- fragment UNBROKEN_ACCENT_QUOTED_CHARACTER_SEQUENCE
--     : GRAVE_ACCENT ACCENT_QUOTED_CHARACTER_REPRESENTATION* GRAVE_ACCENT
--     ;
--
-- fragment SINGLE_QUOTED_CHARACTER_REPRESENTATION:
-- 	(ESCAPED_CHARACTER | ~['\\\r\n])+
-- 	;
--
-- fragment DOUBLE_QUOTED_CHARACTER_REPRESENTATION:
-- 	(ESCAPED_CHARACTER | ~["\\\r\n])+
-- 	;
--
-- fragment ACCENT_QUOTED_CHARACTER_REPRESENTATION:
-- 	(ESCAPED_CHARACTER | ~[`\\\r\n])+
-- 	;
--
-- fragment ESCAPED_CHARACTER
--     : ESCAPED_REVERSE_SOLIDUS
-- 	| ESCAPED_QUOTE
-- 	| ESCAPED_DOUBLE_QUOTE
-- 	| ESCAPED_GRAVE_ACCENT
-- 	| ESCAPED_TAB
-- 	| ESCAPED_BACKSPACE
-- 	| ESCAPED_NEW_LINE
-- 	| ESCAPED_CARRIAGE_RETURN
-- 	| ESCAPED_FORM_FEED
-- 	| ESCAPED_UNICODE4_DIGIT_VALUE
-- 	| ESCAPED_UNICODE6_DIGIT_VALUE
-- 	;
--
-- fragment ESCAPED_REVERSE_SOLIDUS: REVERSE_SOLIDUS REVERSE_SOLIDUS;
-- fragment ESCAPED_QUOTE: REVERSE_SOLIDUS QUOTE;
-- fragment ESCAPED_DOUBLE_QUOTE: REVERSE_SOLIDUS DOUBLE_QUOTE;
-- fragment ESCAPED_GRAVE_ACCENT: REVERSE_SOLIDUS GRAVE_ACCENT;
-- fragment ESCAPED_TAB: REVERSE_SOLIDUS 't';
-- fragment ESCAPED_BACKSPACE: REVERSE_SOLIDUS 'b';
-- fragment ESCAPED_NEW_LINE: REVERSE_SOLIDUS 'n';
-- fragment ESCAPED_CARRIAGE_RETURN: REVERSE_SOLIDUS 'r';
-- fragment ESCAPED_FORM_FEED: REVERSE_SOLIDUS 'f';
-- fragment ESCAPED_UNICODE4_DIGIT_VALUE:
-- 	REVERSE_SOLIDUS 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;
-- fragment ESCAPED_UNICODE6_DIGIT_VALUE:
-- 	REVERSE_SOLIDUS 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;
--
-- // Todo: Finish this. It is tricky how it interacts with <separator>
-- BYTE_STRING_LITERAL
--     : 'X' QUOTE SPACE* (HEX_DIGIT SPACE* HEX_DIGIT SPACE*)* QUOTE
--     ;
      def "ByteStringLiteral"
          string] -- End of Hydra definitions


-- UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_EXACT_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION EXACT_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITHOUT_SUFFIX
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION
--     ;
--
-- UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION APPROXIMATE_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_EXACT_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION EXACT_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITHOUT_SUFFIX
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION
--     ;
--
-- UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION APPROXIMATE_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_INTEGER_WITH_EXACT_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_INTEGER EXACT_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_INTEGER_WITH_APPROXIMATE_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_INTEGER APPROXIMATE_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_INTEGER
--     : DIGIT (UNDERSCORE? DIGIT)*
--     ;
--
-- fragment EXACT_NUMBER_SUFFIX
--     : 'M'
--     ;
--
-- fragment UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION
--     : MANTISSA 'E' EXPONENT
--     ;
--
-- fragment MANTISSA
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION
--     | UNSIGNED_DECIMAL_INTEGER
--     ;
--
-- fragment EXPONENT
--     : SIGNED_DECIMAL_INTEGER
--     ;
--
-- fragment UNSIGNED_DECIMAL_IN_COMMON_NOTATION
--     : UNSIGNED_DECIMAL_INTEGER (PERIOD UNSIGNED_DECIMAL_INTEGER?)
--     | PERIOD UNSIGNED_DECIMAL_INTEGER
--     ;
--
-- fragment SIGNED_DECIMAL_INTEGER
--     : (PLUS_SIGN | MINUS_SIGN)? UNSIGNED_DECIMAL_INTEGER
--     ;
--
-- UNSIGNED_HEXADECIMAL_INTEGER
--     : '0x' ('_'? HEX_DIGIT)+
--     ;
--
-- UNSIGNED_OCTAL_INTEGER
--     : '0o' ('_'? OCTAL_DIGIT)+
--     ;
--
-- UNSIGNED_BINARY_INTEGER
--     : '0b' ('_'? BINARY_DIGIT)+
--     ;
--
-- fragment APPROXIMATE_NUMBER_SUFFIX
--     : 'F'
--     | 'D'
--     ;
--
-- // 21.3 <token>, <separator>, and <identifier>
--
-- // Reserved words
-- ABS: 'ABS';
-- ACOS: 'ACOS';
-- ALL: 'ALL';
-- ALL_DIFFERENT: 'ALL_DIFFERENT';
-- AND: 'AND';
-- ANY: 'ANY';
-- ARRAY: 'ARRAY';
-- AS: 'AS';
-- ASC: 'ASC';
-- ASCENDING: 'ASCENDING';
-- ASIN: 'ASIN';
-- AT: 'AT';
-- ATAN: 'ATAN';
-- AVG: 'AVG';
-- BIG: 'BIG';
-- BIGINT: 'BIGINT';
-- BINARY: 'BINARY';
-- BOOL: 'BOOL';
-- BOOLEAN: 'BOOLEAN';
-- BOTH: 'BOTH';
-- BTRIM: 'BTRIM';
-- BY: 'BY';
-- BYTE_LENGTH: 'BYTE_LENGTH';
-- BYTES: 'BYTES';
-- CALL: 'CALL';
-- CARDINALITY: 'CARDINALITY';
-- CASE: 'CASE';
-- CAST: 'CAST';
-- CEIL: 'CEIL';
-- CEILING: 'CEILING';
-- CHAR: 'CHAR';
-- CHAR_LENGTH: 'CHAR_LENGTH';
-- CHARACTER_LENGTH: 'CHARACTER_LENGTH';
-- CHARACTERISTICS: 'CHARACTERISTICS';
-- CLOSE: 'CLOSE';
-- COALESCE: 'COALESCE';
-- COLLECT_LIST: 'COLLECT_LIST';
-- COMMIT: 'COMMIT';
-- COPY: 'COPY';
-- COS: 'COS';
-- COSH: 'COSH';
-- COT: 'COT';
-- COUNT: 'COUNT';
-- CREATE: 'CREATE';
-- CURRENT_DATE: 'CURRENT_DATE';
-- CURRENT_GRAPH: 'CURRENT_GRAPH';
-- CURRENT_PROPERTY_GRAPH: 'CURRENT_PROPERTY_GRAPH';
-- CURRENT_SCHEMA: 'CURRENT_SCHEMA';
-- CURRENT_TIME: 'CURRENT_TIME';
-- CURRENT_TIMESTAMP: 'CURRENT_TIMESTAMP';
-- DATE: 'DATE';
-- DATETIME: 'DATETIME';
-- DAY: 'DAY';
-- DEC: 'DEC';
-- DECIMAL: 'DECIMAL';
-- DEGREES: 'DEGREES';
-- DELETE: 'DELETE';
-- DESC: 'DESC';
-- DESCENDING: 'DESCENDING';
-- DETACH: 'DETACH';
-- DISTINCT: 'DISTINCT';
-- DOUBLE: 'DOUBLE';
-- DROP: 'DROP';
-- DURATION: 'DURATION';
-- DURATION_BETWEEN: 'DURATION_BETWEEN';
-- ELEMENT_ID: 'ELEMENT_ID';
-- ELSE: 'ELSE';
-- END: 'END';
-- EXCEPT: 'EXCEPT';
-- EXISTS: 'EXISTS';
-- EXP: 'EXP';
-- FILTER: 'FILTER';
-- FINISH: 'FINISH';
-- FLOAT: 'FLOAT';
-- FLOAT16: 'FLOAT16';
-- FLOAT32: 'FLOAT32';
-- FLOAT64: 'FLOAT64';
-- FLOAT128: 'FLOAT128';
-- FLOAT256: 'FLOAT256';
-- FLOOR: 'FLOOR';
-- FOR: 'FOR';
-- FROM: 'FROM';
-- GROUP: 'GROUP';
-- HAVING: 'HAVING';
-- HOME_GRAPH: 'HOME_GRAPH';
-- HOME_PROPERTY_GRAPH: 'HOME_PROPERTY_GRAPH';
-- HOME_SCHEMA: 'HOME_SCHEMA';
-- HOUR: 'HOUR';
-- IF: 'IF';
-- IN: 'IN';
-- INSERT: 'INSERT';
-- INT: 'INT';
-- INTEGER: 'INTEGER';
-- INT8: 'INT8';
-- INTEGER8: 'INTEGER8';
-- INT16: 'INT16';
-- INTEGER16: 'INTEGER16';
-- INT32: 'INT32';
-- INTEGER32: 'INTEGER32';
-- INT64: 'INT64';
-- INTEGER64: 'INTEGER64';
-- INT128: 'INT128';
-- INTEGER128: 'INTEGER128';
-- INT256: 'INT256';
-- INTEGER256: 'INTEGER256';
-- INTERSECT: 'INTERSECT';
-- INTERVAL: 'INTERVAL';
-- IS: 'IS';
-- LEADING: 'LEADING';
-- LEFT: 'LEFT';
-- LET: 'LET';
-- LIKE: 'LIKE';
-- LIMIT: 'LIMIT';
-- LIST: 'LIST';
-- LN: 'LN';
-- LOCAL: 'LOCAL';
-- LOCAL_DATETIME: 'LOCAL_DATETIME';
-- LOCAL_TIME: 'LOCAL_TIME';
-- LOCAL_TIMESTAMP: 'LOCAL_TIMESTAMP';
-- LOG: 'LOG';
-- LOG10: 'LOG10';
-- LOWER: 'LOWER';
-- LTRIM: 'LTRIM';
-- MATCH: 'MATCH';
-- MAX: 'MAX';
-- MIN: 'MIN';
-- MINUTE: 'MINUTE';
-- MOD: 'MOD';
-- MONTH: 'MONTH';
-- NEXT: 'NEXT';
-- NODETACH: 'NODETACH';
-- NORMALIZE: 'NORMALIZE';
-- NOT: 'NOT';
-- NOTHING: 'NOTHING';
-- NULL_KW: 'NULL';            // NULL is a commonly used macro in C++.
-- NULLS: 'NULLS';
-- NULLIF: 'NULLIF';
-- OCTET_LENGTH: 'OCTET_LENGTH';
-- OF: 'OF';
-- OFFSET: 'OFFSET';
-- OPTIONAL: 'OPTIONAL';
-- OR: 'OR';
-- ORDER: 'ORDER';
-- OTHERWISE: 'OTHERWISE';
-- PARAMETER: 'PARAMETER';
-- PARAMETERS: 'PARAMETERS';
-- PATH: 'PATH';
-- PATH_LENGTH: 'PATH_LENGTH';
-- PATHS: 'PATHS';
-- PERCENTILE_CONT: 'PERCENTILE_CONT';
-- PERCENTILE_DISC: 'PERCENTILE_DISC';
-- POWER: 'POWER';
-- PRECISION: 'PRECISION';
-- PROPERTY_EXISTS: 'PROPERTY_EXISTS';
-- RADIANS: 'RADIANS';
-- REAL: 'REAL';
-- RECORD: 'RECORD';
-- REMOVE: 'REMOVE';
-- REPLACE: 'REPLACE';
-- RESET: 'RESET';
-- RETURN: 'RETURN';
-- RIGHT: 'RIGHT';
-- ROLLBACK: 'ROLLBACK';
-- RTRIM: 'RTRIM';
-- SAME: 'SAME';
-- SCHEMA: 'SCHEMA';
-- SECOND: 'SECOND';
-- SELECT: 'SELECT';
-- SESSION: 'SESSION';
-- SESSION_USER: 'SESSION_USER';
-- SET: 'SET';
-- SIGNED: 'SIGNED';
-- SIN: 'SIN';
-- SINH: 'SINH';
-- SIZE: 'SIZE';
-- SKIP_RESERVED_WORD: 'SKIP';
-- SMALL: 'SMALL';
-- SMALLINT: 'SMALLINT';
-- SQRT: 'SQRT';
-- START: 'START';
-- STDDEV_POP: 'STDDEV_POP';
-- STDDEV_SAMP: 'STDDEV_SAMP';
-- STRING: 'STRING';
-- SUM: 'SUM';
-- TAN: 'TAN';
-- TANH: 'TANH';
-- THEN: 'THEN';
-- TIME: 'TIME';
-- TIMESTAMP: 'TIMESTAMP';
-- TRAILING: 'TRAILING';
-- TRIM: 'TRIM';
-- TYPED: 'TYPED';
-- UBIGINT: 'UBIGINT';
-- UINT: 'UINT';
-- UINT8: 'UINT8';
-- UINT16: 'UINT16';
-- UINT32: 'UINT32';
-- UINT64: 'UINT64';
-- UINT128: 'UINT128';
-- UINT256: 'UINT256';
-- UNION: 'UNION';
-- UNSIGNED: 'UNSIGNED';
-- UPPER: 'UPPER';
-- USE: 'USE';
-- USMALLINT: 'USMALLINT';
-- VALUE: 'VALUE';
-- VARBINARY: 'VARBINARY';
-- VARCHAR: 'VARCHAR';
-- VARIABLE: 'VARIABLE';
-- WHEN: 'WHEN';
-- WHERE: 'WHERE';
-- WITH: 'WITH';
-- XOR: 'XOR';
-- YEAR: 'YEAR';
-- YIELD: 'YIELD';
-- ZONED: 'ZONED';
-- ZONED_DATETIME: 'ZONED_DATETIME';
-- ZONED_TIME: 'ZONED_TIME';
--
-- // Prereserved words
-- ABSTRACT: 'ABSTRACT';
-- AGGREGATE: 'AGGREGATE';
-- AGGREGATES: 'AGGREGATES';
-- ALTER: 'ALTER';
-- CATALOG: 'CATALOG';
-- CLEAR: 'CLEAR';
-- CLONE: 'CLONE';
-- CONSTRAINT: 'CONSTRAINT';
-- CURRENT_ROLE: 'CURRENT_ROLE';
-- CURRENT_USER: 'CURRENT_USER';
-- DATA: 'DATA';
-- DIRECTORY: 'DIRECTORY';
-- DRYRUN: 'DRYRUN';
-- EXACT: 'EXACT';
-- EXISTING: 'EXISTING';
-- FUNCTION: 'FUNCTION';
-- GQLSTATUS: 'GQLSTATUS';
-- GRANT: 'GRANT';
-- INSTANT: 'INSTANT';
-- INFINITY_KW: 'INFINITY';            // INFINITY is a commonly used macro in C++
-- NUMBER: 'NUMBER';
-- NUMERIC: 'NUMERIC';
-- ON: 'ON';
-- OPEN: 'OPEN';
-- PARTITION: 'PARTITION';
-- PROCEDURE: 'PROCEDURE';
-- PRODUCT: 'PRODUCT';
-- PROJECT: 'PROJECT';
-- QUERY: 'QUERY';
-- RECORDS: 'RECORDS';
-- REFERENCE: 'REFERENCE';
-- RENAME: 'RENAME';
-- REVOKE: 'REVOKE';
-- SUBSTRING: 'SUBSTRING';
-- SYSTEM_USER: 'SYSTEM_USER';
-- TEMPORAL: 'TEMPORAL';
-- UNIQUE: 'UNIQUE';
-- UNIT: 'UNIT';
-- VALUES: 'VALUES';
--
-- // Nonreserved words
-- ACYCLIC: 'ACYCLIC';
-- BINDING: 'BINDING';
-- BINDINGS: 'BINDINGS';
-- CONNECTING: 'CONNECTING';
-- DESTINATION: 'DESTINATION';
-- DIFFERENT: 'DIFFERENT';
-- DIRECTED: 'DIRECTED';
-- EDGE: 'EDGE';
-- EDGES: 'EDGES';
-- ELEMENT: 'ELEMENT';
-- ELEMENTS: 'ELEMENTS';
-- FIRST: 'FIRST';
-- GRAPH: 'GRAPH';
-- GROUPS: 'GROUPS';
-- KEEP: 'KEEP';
-- LABEL: 'LABEL';
-- LABELED: 'LABELED';
-- LABELS: 'LABELS';
-- LAST: 'LAST';
-- NFC: 'NFC';
-- NFD: 'NFD';
-- NFKC: 'NFKC';
-- NFKD: 'NFKD';
-- NO: 'NO';
-- NODE: 'NODE';
-- NORMALIZED: 'NORMALIZED';
-- ONLY: 'ONLY';
-- ORDINALITY: 'ORDINALITY';
-- PROPERTY: 'PROPERTY';
-- READ: 'READ';
-- RELATIONSHIP: 'RELATIONSHIP';
-- RELATIONSHIPS: 'RELATIONSHIPS';
-- REPEATABLE: 'REPEATABLE';
-- SHORTEST: 'SHORTEST';
-- SIMPLE: 'SIMPLE';
-- SOURCE: 'SOURCE';
-- TABLE: 'TABLE';
-- TEMP: 'TEMP';
-- TO: 'TO';
-- TRAIL: 'TRAIL';
-- TRANSACTION: 'TRANSACTION';
-- TYPE: 'TYPE';
-- UNDIRECTED: 'UNDIRECTED';
-- VERTEX: 'VERTEX';
-- WALK: 'WALK';
-- WITHOUT: 'WITHOUT';
-- WRITE: 'WRITE';
-- ZONE: 'ZONE';
--
-- fragment SEPARATED_IDENTIFIER
--     : DELIMITED_IDENTIFIER
--     | EXTENDED_IDENTIFIER
--     ;
--
-- REGULAR_IDENTIFIER
--     : IDENTIFIER_START IDENTIFIER_EXTEND*
--     ;
--
-- fragment EXTENDED_IDENTIFIER
--     : IDENTIFIER_EXTEND+
--     ;
--
-- fragment DELIMITED_IDENTIFIER
--     : DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     | ACCENT_QUOTED_CHARACTER_SEQUENCE
--     ;
--
-- SUBSTITUTED_PARAMETER_REFERENCE
--     : DOUBLE_DOLLAR_SIGN PARAMETER_NAME
--     ;
--
-- GENERAL_PARAMETER_REFERENCE
--     : DOLLAR_SIGN PARAMETER_NAME
--     ;
--
-- fragment IDENTIFIER_START
--     : ID_Start
--     | Pc
--     ;
--
-- fragment IDENTIFIER_EXTEND
--     : ID_Continue
--     ;
--
-- fragment ID_Start
--     : [\p{ID_Start}]
--     ;
--
-- fragment ID_Continue
--     : [\p{ID_Continue}]
--     ;
--
-- MULTISET_ALTERNATION_OPERATOR: '|+|';
--
-- BRACKET_RIGHT_ARROW: ']->';
-- BRACKET_TILDE_RIGHT_ARROW: ']~>';
-- CONCATENATION_OPERATOR: '||';
-- DOUBLE_COLON: '::';
-- DOUBLE_DOLLAR_SIGN: '$$';
-- DOUBLE_PERIOD: '..';
-- GREATER_THAN_OR_EQUALS_OPERATOR: '>=';
-- LEFT_ARROW: '<-';
-- LEFT_ARROW_TILDE: '<~';
-- LEFT_ARROW_BRACKET: '<-[';
-- LEFT_ARROW_TILDE_BRACKET: '<~[';
-- LEFT_MINUS_RIGHT: '<->';
-- LEFT_MINUS_SLASH: '<-/';
-- LEFT_TILDE_SLASH: '<~/';
-- LESS_THAN_OR_EQUALS_OPERATOR: '<=';
-- MINUS_LEFT_BRACKET: '-[';
-- MINUS_SLASH: '-/';
-- NOT_EQUALS_OPERATOR: '<>';
-- RIGHT_ARROW: '->';
-- RIGHT_BRACKET_MINUS: ']-';
-- RIGHT_BRACKET_TILDE: ']~';
-- RIGHT_DOUBLE_ARROW: '=>';
-- SLASH_MINUS: '/-';
-- SLASH_MINUS_RIGHT: '/->';
-- SLASH_TILDE: '/~';
-- SLASH_TILDE_RIGHT: '/~>';
-- TILDE_LEFT_BRACKET: '~[';
-- TILDE_RIGHT_ARROW: '~>';
-- TILDE_SLASH: '~/';
--
-- // 21.4 GQL terminal characters
--
-- AMPERSAND: '&';
-- ASTERISK: '*';
-- COLON: ':';
-- COMMA: ',';
-- COMMERCIAL_AT: '@';
-- DOLLAR_SIGN: '$';
-- DOUBLE_QUOTE: '"';
-- EQUALS_OPERATOR: '=';
-- EXCLAMATION_MARK: '!';
-- RIGHT_ANGLE_BRACKET: '>';
-- GRAVE_ACCENT: '`';
-- LEFT_BRACE: '{';
-- LEFT_BRACKET: '[';
-- LEFT_PAREN: '(';
-- LEFT_ANGLE_BRACKET: '<';
-- MINUS_SIGN: '-';
-- PERCENT: '%';
-- PERIOD: '.';
-- PLUS_SIGN: '+';
-- QUESTION_MARK: '?';
-- QUOTE: '\'';
-- REVERSE_SOLIDUS: '\\';
-- RIGHT_BRACE: '}';
-- RIGHT_BRACKET: ']';
-- RIGHT_PAREN: ')';
-- SOLIDUS: '/';
-- TILDE: '~';
-- UNDERSCORE: '_';
-- VERTICAL_BAR: '|';
--
-- fragment HEX_DIGIT
--     : [0-9a-f]
--     ;
--
-- fragment DIGIT
--     : [0-9]
--     ;
--
-- fragment OCTAL_DIGIT
--     : [0-7]
--     ;
--
-- fragment BINARY_DIGIT
--     : [0-1]
--     ;
--
-- SP
--   : (WHITESPACE)+
--   -> channel(HIDDEN)
--   ;
--
-- WHITESPACE
--     : SPACE
--     | TAB
--     | LF
--     | VT
--     | FF
--     | CR
--     | FS
--     | GS
--     | RS
--     | US
--     | '\u1680'
--     | '\u180e'
--     | '\u2000'
--     | '\u2001'
--     | '\u2002'
--     | '\u2003'
--     | '\u2004'
--     | '\u2005'
--     | '\u2006'
--     | '\u2008'
--     | '\u2009'
--     | '\u200a'
--     | '\u2028'
--     | '\u2029'
--     | '\u205f'
--     | '\u3000'
--     | '\u00a0'
--     | '\u2007'
--     | '\u202f'
--     ;
--
-- BRACKETED_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
--
-- SIMPLE_COMMENT_SOLIDUS: '//' ~[\r\n]* -> channel(HIDDEN);
--
-- SIMPLE_COMMENT_MINUS: '--' ~[\r\n]* -> channel(HIDDEN);
--
-- fragment GS : [\u001D];
--
-- fragment FS : [\u001C];
--
-- fragment CR : [\r];
--
-- fragment Sc : [\p{Sc}];
--
-- fragment SPACE : [ ];
--
-- fragment Pc : [\p{Pc}];
--
-- fragment TAB : [\t];
--
-- fragment LF : [\n];
--
-- fragment VT : [\u000B];
--
-- fragment US : [\u001F];
--
-- fragment FF: [\f];
--
-- fragment RS: [\u001E];
