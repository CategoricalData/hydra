{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Cypher.OpenCypher where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


openCypherModule :: Module
openCypherModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A Cypher model based on the OpenCypher specification (version 23), copyright Neo Technology, available at:\n" ++
      "  https://opencypher.org/resources/")
  where
    ns = Namespace "hydra.ext.cypher.openCypher"
    def = datatype ns
    cypher = typeref ns

    elements = [

-- Cypher = [SP], Statement, [[SP], ';'], [SP], EOI ;
-- 
-- Statement = Query ;
-- 
-- Query = RegularQuery
--       | StandaloneCall
--       ;

      def "Query" $
        union [
          "regular">: cypher "RegularQuery",
          "standalone">: cypher "StandaloneCall"],

-- RegularQuery = SingleQuery, { [SP], Union } ;

      def "RegularQuery" $
        record [
          "head">: cypher "SingleQuery",
          "rest">: list $ cypher "Union"],

-- Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
--       | ((U,N,I,O,N), [SP], SingleQuery)
--       ;

      def "Union" $
        record [
          "all">: boolean,
          "query">: cypher "SingleQuery"],

-- SingleQuery = SinglePartQuery
--             | MultiPartQuery
--             ;
      def "SingleQuery" $
        union [
          "singlePart">: cypher "SinglePartQuery",
          "multiPart">: cypher "MultiPartQuery"],

-- SinglePartQuery = ({ ReadingClause, [SP] }, Return)
--                 | ({ ReadingClause, [SP] }, UpdatingClause, { [SP], UpdatingClause }, [[SP], Return])
--                 ;

      def "SinglePartQuery" $
        record [
          "reading">: list $ cypher "ReadingClause",
          "updating">: list $ cypher "UpdatingClause",
          "return">: optional $ cypher "Return"],
          
-- MultiPartQuery = { { ReadingClause, [SP] }, { UpdatingClause, [SP] }, With, [SP] }-, SinglePartQuery ;
          
      def "WithClause" $
        record [
          "reading">: list $ cypher "ReadingClause",
          "updating">: list $ cypher "UpdatingClause",
          "with">: cypher "With"],

      def "MultiPartQuery" $
        record [
          "with">: list $ cypher "WithClause",
          "body">: cypher "SinglePartQuery"],

-- UpdatingClause = Create
--                | Merge
--                | Delete
--                | Set
--                | Remove
--                ;

      def "UpdatingClause" $
        union [
          "create">: cypher "Create",
          "merge">: cypher "Merge",
          "delete">: cypher "Delete",
          "set">: cypher "Set",
          "remove">: cypher "Remove"],

-- ReadingClause = Match
--               | Unwind
--               | InQueryCall
--               ;

      def "ReadingClause" $
        union [
          "match">: cypher "Match",
          "unwind">: cypher "Unwind",
          "inQueryCall">: cypher "InQueryCall"],
          
-- Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), [SP], Pattern, [[SP], Where] ;

      def "Match" $
        record [
          "optional">: boolean,
          "pattern">: cypher "Pattern",
          "where">: optional $ cypher "Where"],

-- Unwind = (U,N,W,I,N,D), [SP], Expression, SP, (A,S), SP, Variable ;

      def "Unwind" $
        record [
          "expression">: cypher "Expression",
          "variable">: cypher "Variable"],

-- Merge = (M,E,R,G,E), [SP], PatternPart, { SP, MergeAction } ;

      def "Merge" $
        record [
          "patternPart">: cypher "PatternPart",
          "actions">: list $ cypher "MergeAction"],
          
-- MergeAction = ((O,N), SP, (M,A,T,C,H), SP, Set)
--             | ((O,N), SP, (C,R,E,A,T,E), SP, Set)
--             ;

      def "MatchOrCreate" $
        enum ["match", "create"],
        
      def "MergeAction" $
        record [
          "action">: cypher "MatchOrCreate",
          "set">: cypher "Set"],
          
-- Create = (C,R,E,A,T,E), [SP], Pattern ;

      def "Create" $
        wrap $ cypher "Pattern",

-- Set = (S,E,T), [SP], SetItem, { [SP], ',', [SP], SetItem } ;

      def "Set" $
        wrap $ nonemptyList $ cypher "SetItem",

-- SetItem = (PropertyExpression, [SP], '=', [SP], Expression)
--         | (Variable, [SP], '=', [SP], Expression)
--         | (Variable, [SP], '+=', [SP], Expression)
--         | (Variable, [SP], NodeLabels)
--         ;

      def "SetItem" $
        union [
          "property">: cypher "PropertyEquals",
          "variableEqual">: cypher "VariableEquals",
          "variablePlusEqual">: cypher "VariablePlusEquals",
          "variableLabels">: cypher "VariableAndNodeLabels"],

      def "PropertyEquals" $
        record [
          "lhs">: cypher "PropertyExpression",
          "rhs">: cypher "Expression"],

      def "VariableEquals" $
        record [
          "lhs">: cypher "Variable",
          "rhs">: cypher "Expression"],

      def "VariablePlusEquals" $
        record [
          "lhs">: cypher "Variable",
          "rhs">: cypher "Expression"],

      def "VariableAndNodeLabels" $
        record [
          "variable">: cypher "Variable",
          "labels">: cypher "NodeLabels"],

-- Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;

      def "Delete" $
        record [
          "detach">: boolean,
          "expressions">: nonemptyList $ cypher "Expression"],

-- Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;

      def "Remove" $
        wrap $ nonemptyList $ cypher "RemoveItem",

-- RemoveItem = (Variable, NodeLabels)
--            | PropertyExpression
--            ;

      def "RemoveItem" $
        union [
          "variableLabels">: cypher "VariableAndNodeLabels",
          "property">: cypher "PropertyExpression"],

-- InQueryCall = (C,A,L,L), SP, ExplicitProcedureInvocation, [[SP], (Y,I,E,L,D), SP, YieldItems] ;

      def "InQueryCall" $
        record [
          "call">: cypher "ExplicitProcedureInvocation",
          "yieldItems">: optional $ cypher "YieldItems"],

-- StandaloneCall = (C,A,L,L), SP, (ExplicitProcedureInvocation | ImplicitProcedureInvocation), [[SP], (Y,I,E,L,D), SP, ('*' | YieldItems)] ;

      def "ProcedureInvocation" $
        union [
          "explicit">: cypher "ExplicitProcedureInvocation",
          "implicit">: cypher "ImplicitProcedureInvocation"],

      def "StarOrYieldItems" $
        union [
          "star">: unit,
          "items">: cypher "YieldItems"],

      def "StandaloneCall" $
        record [
          "call">: cypher "ProcedureInvocation",
          "yieldItems">: optional $ cypher "StarOrYieldItems"],

-- YieldItems = YieldItem, { [SP], ',', [SP], YieldItem }, [[SP], Where] ;

      def "YieldItems" $
        record [
          "items">: nonemptyList $ cypher "YieldItem",
          "where">: optional $ cypher "Where"],

-- YieldItem = [ProcedureResultField, SP, (A,S), SP], Variable ;

      def "YieldItem" $
        record [
          "field">: optional $ cypher "ProcedureResultField",
          "variable">: cypher "Variable"],

-- With = (W,I,T,H), ProjectionBody, [[SP], Where] ;

      def "With" $
        record [
          "projection">: cypher "ProjectionBody",
          "where">: optional $ cypher "Where"],

-- Return = (R,E,T,U,R,N), ProjectionBody ;

      def "Return" $
        wrap $ cypher "ProjectionBody",

-- ProjectionBody = [[SP], (D,I,S,T,I,N,C,T)], SP, ProjectionItems, [SP, Order], [SP, Skip], [SP, Limit] ;

      def "ProjectionBody" $
        record [
          "distinct">: boolean,
          "projectionItems">: cypher "ProjectionItems",
          "order">: optional $ cypher "Order",
          "skip">: optional $ cypher "Skip",
          "limit">: optional $ cypher "Limit"],

-- ProjectionItems = ('*', { [SP], ',', [SP], ProjectionItem })
--                 | (ProjectionItem, { [SP], ',', [SP], ProjectionItem })
--                 ;

      def "ProjectionItems" $
        record [
          "star">: boolean,
          "explicit">: list $ cypher "ProjectionItem"],

-- ProjectionItem = (Expression, SP, (A,S), SP, Variable)
--                | Expression
--                ;

        def "ProjectionItem" $
          record [
            "expression">: cypher "Expression",
            "variable">: optional $ cypher "Variable"],

-- Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;

      def "Order" $
        wrap $ nonemptyList $ cypher "SortItem",

-- Skip = (S,K,I,P), SP, Expression ;

      def "Skip" $
        wrap $ cypher "Expression",

-- Limit = (L,I,M,I,T), SP, Expression ;

      def "Limit" $
        wrap $ cypher "Expression",

-- SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;

      def "SortOrder" $
        enum ["ascending", "descending"],

      def "SortItem" $
        record [
          "expression">: cypher "Expression",
          "order">: optional $ cypher "SortOrder"],

-- Where = (W,H,E,R,E), SP, Expression ;

      def "Where" $
        wrap $ cypher "Expression",

-- Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;

      def "Pattern" $
        wrap $ nonemptyList $ cypher "PatternPart",

-- PatternPart = (Variable, [SP], '=', [SP], AnonymousPatternPart)
--             | AnonymousPatternPart
--             ;

      def "PatternPart" $
        record [
          "variable">: optional $ cypher "Variable",
          "pattern">: cypher "AnonymousPatternPart"],

-- AnonymousPatternPart = PatternElement ;

        def "AnonymousPatternPart" $
          wrap $ cypher "PatternElement",

-- PatternElement = (NodePattern, { [SP], PatternElementChain })
--                | ('(', PatternElement, ')')
--                ;

      def "NodePatternChain" $
        record [
          "nodePattern">: cypher "NodePattern",
          "chain">: list $ cypher "PatternElementChain"],

      def "PatternElement" $
        union [
          "chained">: cypher "NodePatternChain",
          "parenthesized">: cypher "PatternElement"],

-- RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;

      def "RelationshipsPattern" $
        record [
          "nodePattern">: cypher "NodePattern",
          "chain">: nonemptyList $ cypher "PatternElementChain"],

-- NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
      def "NodePattern" $
        record [
          "variable">: optional $ cypher "Variable",
          "labels">: optional $ cypher "NodeLabels",
          "properties">: optional $ cypher "Properties"],

-- PatternElementChain = RelationshipPattern, [SP], NodePattern ;

        def "PatternElementChain" $
          record [
            "relationship">: cypher "RelationshipPattern",
            "node">: cypher "NodePattern"],

-- RelationshipPattern = (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
--                     | (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash)
--                     | (Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
--                     | (Dash, [SP], [RelationshipDetail], [SP], Dash)
--                     ;

        def "RelationshipPattern" $
          record [
            "leftArrow">: boolean,
            "detail">: optional $ cypher "RelationshipDetail",
            "rightArrow">: boolean],

-- RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;

      def "RelationshipDetail" $
        record [
          "variable">: optional $ cypher "Variable",
          "types">: optional $ cypher "RelationshipTypes",
          "range">: optional $ cypher "RangeLiteral",
          "properties">: optional $ cypher "Properties"],

-- Properties = MapLiteral
--            | Parameter
--            ;

      def "Properties" $
        union [
          "map">: cypher "MapLiteral",
          "parameter">: cypher "Parameter"],

-- RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;

      -- TODO: check whether the slight difference in colon syntax is significant
      def "RelationshipTypes" $
        wrap $ nonemptyList $ cypher "RelTypeName",

-- NodeLabels = NodeLabel, { [SP], NodeLabel } ;

      def "NodeLabels" $
        wrap $ nonemptyList $ cypher "NodeLabel",

-- NodeLabel = ':', [SP], LabelName ;

      def "NodeLabel" $
        wrap string,

-- RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;

      def "RangeLiteral" $
        record [
          "start">: optional bigint,
          "end">: optional bigint],

-- LabelName = SchemaName ;
-- 
-- RelTypeName = SchemaName ;

      def "RelTypeName" $
        wrap string,

-- PropertyExpression = Atom, { [SP], PropertyLookup }- ;

      def "PropertyExpression" $
        record [
          "atom">: cypher "Atom",
          "lookups">: nonemptyList $ cypher "PropertyLookup"],

-- Expression = OrExpression ;

      def "Expression" $
        wrap $ cypher "OrExpression",

-- OrExpression = XorExpression, { SP, (O,R), SP, XorExpression } ;

      def "OrExpression" $
        wrap $ nonemptyList $ cypher "XorExpression",

-- XorExpression = AndExpression, { SP, (X,O,R), SP, AndExpression } ;

      def "XorExpression" $
        wrap $ nonemptyList $ cypher "AndExpression",

-- AndExpression = NotExpression, { SP, (A,N,D), SP, NotExpression } ;

      def "AndExpression" $
        wrap $ nonemptyList $ cypher "NotExpression",

-- NotExpression = { (N,O,T), [SP] }, ComparisonExpression ;

      def "NotExpression" $
        record [
          "not">: boolean,
          "expression">: cypher "ComparisonExpression"],

-- ComparisonExpression = StringListNullPredicateExpression, { [SP], PartialComparisonExpression } ;

      def "ComparisonExpression" $
        record [
          "left">: cypher "StringListNullPredicateExpression",
          "right">: list $ cypher "PartialComparisonExpression"],

-- PartialComparisonExpression = ('=', [SP], StringListNullPredicateExpression)
--                             | ('<>', [SP], StringListNullPredicateExpression)
--                             | ('<', [SP], StringListNullPredicateExpression)
--                             | ('>', [SP], StringListNullPredicateExpression)
--                             | ('<=', [SP], StringListNullPredicateExpression)
--                             | ('>=', [SP], StringListNullPredicateExpression)
--                             ;

      def "ComparisonOperator" $
        enum [
          "eq",
          "neq",
          "lt",
          "gt",
          "lte",
          "gte"],

      def "PartialComparisonExpression" $
        record [
          "operator">: cypher "ComparisonOperator",
          "right">: cypher "StringListNullPredicateExpression"],

-- StringListNullPredicateExpression = AddOrSubtractExpression, { StringPredicateExpression | ListPredicateExpression | NullPredicateExpression } ;

      def "StringListNullPredicateExpression" $
        record [
          "left">: cypher "AddOrSubtractExpression",
          "right">: list $ cypher "StringListNullPredicateRightHandSide"],

      def "StringListNullPredicateRightHandSide" $
        union [
          "string">: cypher "StringPredicateExpression",
          "list">: cypher "ListPredicateExpression",
          "null">: cypher "NullPredicateExpression"],

-- StringPredicateExpression = ((SP, (S,T,A,R,T,S), SP, (W,I,T,H)) | (SP, (E,N,D,S), SP, (W,I,T,H)) | (SP, (C,O,N,T,A,I,N,S))), [SP], AddOrSubtractExpression ;

      def "StringPredicateExpression" $
        record [
          "operator">: cypher "StringPredicateOperator",
          "expression">: cypher "AddOrSubtractExpression"],

      def "StringPredicateOperator" $
        enum [
          "startsWith",
          "endsWith",
          "contains"],

-- ListPredicateExpression = SP, (I,N), [SP], AddOrSubtractExpression ;

      def "ListPredicateExpression" $
        wrap $ cypher "AddOrSubtractExpression",

-- NullPredicateExpression = (SP, (I,S), SP, (N,U,L,L))
--                         | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L))
--                         ;

      def "NullPredicateExpression" $
        wrap $ boolean, -- true: NULL, false: NOT NULL

-- AddOrSubtractExpression = MultiplyDivideModuloExpression, { ([SP], '+', [SP], MultiplyDivideModuloExpression) | ([SP], '-', [SP], MultiplyDivideModuloExpression) } ;

      def "AddOrSubtractExpression" $
        record [
          "left">: cypher "MultiplyDivideModuloExpression",
          "right">: list $ cypher "AddOrSubtractRightHandSide"],

      def "AddOrSubtractRightHandSide" $
        record [
          "operator">: cypher "AddOrSubtractOperator",
          "expression">: cypher "MultiplyDivideModuloExpression"],

      def "AddOrSubtractOperator" $
        enum [
          "add",
          "subtract"],

-- MultiplyDivideModuloExpression = PowerOfExpression, { ([SP], '*', [SP], PowerOfExpression) | ([SP], '/', [SP], PowerOfExpression) | ([SP], '%', [SP], PowerOfExpression) } ;

      def "MultiplyDivideModuloExpression" $
        record [
          "left">: cypher "PowerOfExpression",
          "right">: list $ cypher "MultiplyDivideModuloRightHandSide"],

      def "MultiplyDivideModuloRightHandSide" $
        record [
          "operator">: cypher "MultiplyDivideModuloOperator",
          "expression">: cypher "PowerOfExpression"],

      def "MultiplyDivideModuloOperator" $
        enum [
          "multiply",
          "divide",
          "modulo"],

-- PowerOfExpression = UnaryAddOrSubtractExpression, { [SP], '^', [SP], UnaryAddOrSubtractExpression } ;

      def "PowerOfExpression" $
        wrap $ nonemptyList $ cypher "UnaryAddOrSubtractExpression",

-- UnaryAddOrSubtractExpression = NonArithmeticOperatorExpression
--                              | (('+' | '-'), [SP], NonArithmeticOperatorExpression)
--                              ;

      def "UnaryAddOrSubtractExpression" $
        record [
          "operator">: optional $ cypher "AddOrSubtractOperator",
          "expression">: cypher "NonArithmeticOperatorExpression"],

-- NonArithmeticOperatorExpression = Atom, { ([SP], ListOperatorExpression) | ([SP], PropertyLookup) }, [[SP], NodeLabels] ;

      def "ListOperatorExpressionOrPropertyLookup" $
        union [
          "list">: cypher "ListOperatorExpression",
          "property">: cypher "PropertyLookup"],

      def "NonArithmeticOperatorExpression" $
        record [
          "atom">: cypher "Atom",
          "listsAndLookups">: list $ cypher "ListOperatorExpressionOrPropertyLookup",
          "labels">: optional $ cypher "NodeLabels"],

-- ListOperatorExpression = ('[', Expression, ']')
--                        | ('[', [Expression], '..', [Expression], ']')
--                        ;

      def "RangeExpression" $
         record [
            "start">: optional $ cypher "Expression",
            "end">: optional $ cypher "Expression"],

      def "ListOperatorExpression" $
        union [
          "single">: cypher "Expression",
          "range">: cypher "RangeExpression"],

-- PropertyLookup = '.', [SP], (PropertyKeyName) ;

      def "PropertyLookup" $
        wrap $ cypher "PropertyKeyName",

-- Atom = Literal
--      | Parameter
--      | CaseExpression
--      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
--      | ListComprehension
--      | PatternComprehension
--      | Quantifier
--      | PatternPredicate
--      | ParenthesizedExpression
--      | FunctionInvocation
--      | ExistentialSubquery
--      | Variable
--      ;

      def "Atom" $
        union [
          "literal">: cypher "Literal",
          "parameter">: cypher "Parameter",
          "case">: cypher "CaseExpression",
          "countStar">: unit,
          "listComprehension">: cypher "ListComprehension",
          "patternComprehension">: cypher "PatternComprehension",
          "quantifier">: cypher "Quantifier",
          "patternPredicate">: cypher "PatternPredicate",
          "parenthesized">: cypher "ParenthesizedExpression",
          "functionInvocation">: cypher "FunctionInvocation",
          "existentialSubquery">: cypher "ExistentialSubquery",
          "variable">: cypher "Variable"],

-- CaseExpression = (((C,A,S,E), { [SP], CaseAlternative }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternative }-)), [[SP], (E,L,S,E), [SP], Expression], [SP], (E,N,D) ;

      def "CaseExpression" $
        record [
          "expression">: optional $ cypher "Expression",
          "alternatives">: nonemptyList $ cypher "CaseAlternative",
          "else">: optional $ cypher "Expression"],

-- CaseAlternative = (W,H,E,N), [SP], Expression, [SP], (T,H,E,N), [SP], Expression ;

      def "CaseAlternative" $
        record [
          "condition">: cypher "Expression",
          "result">: cypher "Expression"],

-- ListComprehension = '[', [SP], FilterExpression, [[SP], '|', [SP], Expression], [SP], ']' ;

      def "ListComprehension" $
        record [
          "left">: cypher "FilterExpression",
          "right">: optional $ cypher "Expression"],

-- PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [Where, [SP]], '|', [SP], Expression, [SP], ']' ;

      def "PatternComprehension" $
        record [
          "variable">: optional $ cypher "Variable",
          "pattern">: cypher "RelationshipsPattern",
          "where">: optional $ cypher "Where",
          "right">: cypher "Expression"],

-- Quantifier = ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
--            | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
--            | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
--            | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
--            ;

      def "Quantifier" $
        record [
          "operator">: cypher "QuantifierOperator",
          "expression">: cypher "FilterExpression"],

      def "QuantifierOperator" $
        enum [
          "all",
          "any",
          "none",
          "single"],

-- FilterExpression = IdInColl, [[SP], Where] ;

      def "FilterExpression" $
        record [
          "idInColl">: cypher "IdInColl",
          "where">: optional $ cypher "Where"],

-- PatternPredicate = RelationshipsPattern ;

      def "PatternPredicate" $
        wrap $ cypher "RelationshipsPattern",

-- ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;

      def "ParenthesizedExpression" $
        wrap $ cypher "Expression",

-- IdInColl = Variable, SP, (I,N), SP, Expression ;

      def "IdInColl" $
        record [
          "variable">: cypher "Variable",
          "expression">: cypher "Expression"],

-- FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

      def "FunctionInvocation" $
        record [
          "name">: cypher "QualifiedName",
          "distinct">: boolean,
          "arguments">: list $ cypher "Expression"],

-- FunctionName = Namespace, SymbolicName ;

      def "QualifiedName" $
        record [
          "namespace">: string,
          "local">: string],

-- ExistentialSubquery = (E,X,I,S,T,S), [SP], '{', [SP], (RegularQuery | (Pattern, [[SP], Where])), [SP], '}' ;

      def "PatternWhere" $
        record [
          "pattern">: cypher "Pattern",
          "where">: optional $ cypher "Where"],

      def "ExistentialSubquery" $
        union [
          "regular">: cypher "RegularQuery",
          "pattern">: cypher "PatternWhere"],

-- ExplicitProcedureInvocation = ProcedureName, [SP], '(', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

      def "ExplicitProcedureInvocation" $
        record [
          "name">: cypher "QualifiedName",
          "arguments">: list $ cypher "Expression"],
          
-- ImplicitProcedureInvocation = ProcedureName ;

      def "ImplicitProcedureInvocation" $
        wrap $ cypher "QualifiedName",

-- ProcedureResultField = SymbolicName ;

      def "ProcedureResultField" $
        wrap string,

-- ProcedureName = Namespace, SymbolicName ;
--        
-- Namespace = { SymbolicName, '.' } ;
-- 
-- Variable = SymbolicName ;

      def "Variable" $
        wrap string,

-- Literal = BooleanLiteral
--         | (N,U,L,L)
--         | NumberLiteral
--         | StringLiteral
--         | ListLiteral
--         | MapLiteral
--         ;

      def "Literal" $
        union [
          "boolean">: boolean,
          "null">: unit,
          "number">: cypher "NumberLiteral",
          "string">: cypher "StringLiteral",
          "list">: cypher "ListLiteral",
          "map">: cypher "MapLiteral"],
          
-- BooleanLiteral = (T,R,U,E)
--                | (F,A,L,S,E)
--                ;
-- 
-- NumberLiteral = DoubleLiteral
--               | IntegerLiteral
--               ;

      def "NumberLiteral" $
        union [
          "double">: float64,
          "integer">: bigint],

-- IntegerLiteral = HexInteger
--                | OctalInteger
--                | DecimalInteger
--                ;
-- 
-- HexInteger = '0x', { HexDigit }- ;
-- 
-- DecimalInteger = ZeroDigit
--                | (NonZeroDigit, { Digit })
--                ;
-- 
-- OctalInteger = '0o', { OctDigit }- ;
-- 
-- HexLetter = (A)
--           | (B)
--           | (C)
--           | (D)
--           | (E)
--           | (F)
--           ;
-- 
-- HexDigit = Digit
--          | HexLetter
--          ;
-- 
-- Digit = ZeroDigit
--       | NonZeroDigit
--       ;
-- 
-- NonZeroDigit = NonZeroOctDigit
--              | '8'
--              | '9'
--              ;
-- 
-- NonZeroOctDigit = '1'
--                 | '2'
--                 | '3'
--                 | '4'
--                 | '5'
--                 | '6'
--                 | '7'
--                 ;
-- 
-- OctDigit = ZeroDigit
--          | NonZeroOctDigit
--          ;
-- 
-- ZeroDigit = '0' ;
-- 
-- DoubleLiteral = ExponentDecimalReal
--               | RegularDecimalReal
--               ;
-- 
-- ExponentDecimalReal = ({ Digit }- | ({ Digit }-, '.', { Digit }-) | ('.', { Digit }-)), (E), ['-'], { Digit }- ;
-- 
-- RegularDecimalReal = { Digit }, '.', { Digit }- ;
-- 
-- StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
--               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'")
--               ;

      def "StringLiteral" $
        wrap string,

-- EscapedChar = '\', ('\' | "'" | '"' | (B) | (F) | (N) | (R) | (T) | ((U), 4 * HexDigit) | ((U), 8 * HexDigit)) ;
-- 
-- ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;

      def "ListLiteral" $
        wrap $ list $ cypher "Expression",

-- MapLiteral = '{', [SP], [PropertyKeyName, [SP], ':', [SP], Expression, [SP], { ',', [SP], PropertyKeyName, [SP], ':', [SP], Expression, [SP] }], '}' ;

      def "MapLiteral" $
        wrap $ list $ cypher "KeyValuePair",

      def "KeyValuePair" $
        record [
          "key">: cypher "PropertyKeyName",
          "value">: cypher "Expression"],
          
-- PropertyKeyName = SchemaName ;

      def "PropertyKeyName" $
        wrap string,

-- Parameter = '$', (SymbolicName | DecimalInteger) ;

      def "Parameter" $
        union [
          "symbolic">: string,
          "integer">: bigint]]

-- SchemaName = SymbolicName
--            | ReservedWord
--            ;
-- 
-- ReservedWord = (A,L,L)
--              | (A,S,C)
--              | (A,S,C,E,N,D,I,N,G)
--              | (B,Y)
--              | (C,R,E,A,T,E)
--              | (D,E,L,E,T,E)
--              | (D,E,S,C)
--              | (D,E,S,C,E,N,D,I,N,G)
--              | (D,E,T,A,C,H)
--              | (E,X,I,S,T,S)
--              | (L,I,M,I,T)
--              | (M,A,T,C,H)
--              | (M,E,R,G,E)
--              | (O,N)
--              | (O,P,T,I,O,N,A,L)
--              | (O,R,D,E,R)
--              | (R,E,M,O,V,E)
--              | (R,E,T,U,R,N)
--              | (S,E,T)
--              | (S,K,I,P)
--              | (W,H,E,R,E)
--              | (W,I,T,H)
--              | (U,N,I,O,N)
--              | (U,N,W,I,N,D)
--              | (A,N,D)
--              | (A,S)
--              | (C,O,N,T,A,I,N,S)
--              | (D,I,S,T,I,N,C,T)
--              | (E,N,D,S)
--              | (I,N)
--              | (I,S)
--              | (N,O,T)
--              | (O,R)
--              | (S,T,A,R,T,S)
--              | (X,O,R)
--              | (F,A,L,S,E)
--              | (T,R,U,E)
--              | (N,U,L,L)
--              | (C,O,N,S,T,R,A,I,N,T)
--              | (D,O)
--              | (F,O,R)
--              | (R,E,Q,U,I,R,E)
--              | (U,N,I,Q,U,E)
--              | (C,A,S,E)
--              | (W,H,E,N)
--              | (T,H,E,N)
--              | (E,L,S,E)
--              | (E,N,D)
--              | (M,A,N,D,A,T,O,R,Y)
--              | (S,C,A,L,A,R)
--              | (O,F)
--              | (A,D,D)
--              | (D,R,O,P)
--              ;
-- 
-- SymbolicName = UnescapedSymbolicName
--              | EscapedSymbolicName
--              | HexLetter
--              | (C,O,U,N,T)
--              | (F,I,L,T,E,R)
--              | (E,X,T,R,A,C,T)
--              | (A,N,Y)
--              | (N,O,N,E)
--              | (S,I,N,G,L,E)
--              ;
-- 
-- UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;
-- 
-- (* Based on the unicode identifier and pattern syntax
--  *   (http://www.unicode.org/reports/tr31/)
--  * And extended with a few characters.
--  *)IdentifierStart = ID_Start
--                 | Pc
--                 ;
-- 
-- (* Based on the unicode identifier and pattern syntax
--  *   (http://www.unicode.org/reports/tr31/)
--  * And extended with a few characters.
--  *)IdentifierPart = ID_Continue
--                | Sc
--                ;
-- 
-- (* Any character except "`", enclosed within `backticks`. Backticks are escaped with double backticks.
--  *)EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
-- 
-- SP = { whitespace }- ;
-- 
-- whitespace = SPACE
--            | TAB
--            | LF
--            | VT
--            | FF
--            | CR
--            | FS
--            | GS
--            | RS
--            | US
--            | ' '
--            | '᠎'
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | ' '
--            | '　'
--            | ' '
--            | ' '
--            | ' '
--            | Comment
--            ;
-- 
-- Comment = ('/*', { ANY - ('*') | ('*', ANY - ('/')) }, '*/')
--         | ('//', { ANY - (LF | CR) }, [CR], (LF | EOI))
--         ;
-- 
-- LeftArrowHead = '<'
--               | '⟨'
--               | '〈'
--               | '﹤'
--               | '＜'
--               ;
-- 
-- RightArrowHead = '>'
--                | '⟩'
--                | '〉'
--                | '﹥'
--                | '＞'
--                ;
-- 
-- Dash = '-'
--      | '­'
--      | '‐'
--      | '‑'
--      | '‒'
--      | '–'
--      | '—'
--      | '―'
--      | '−'
--      | '﹘'
--      | '﹣'
--      | '－'
--      ;
-- 
-- A = 'A' | 'a' ;
-- B = 'B' | 'b' ;
-- C = 'C' | 'c' ;
-- D = 'D' | 'd' ;
-- E = 'E' | 'e' ;
-- F = 'F' | 'f' ;
-- G = 'G' | 'g' ;
-- H = 'H' | 'h' ;
-- I = 'I' | 'i' ;
-- K = 'K' | 'k' ;
-- L = 'L' | 'l' ;
-- M = 'M' | 'm' ;
-- N = 'N' | 'n' ;
-- O = 'O' | 'o' ;
-- P = 'P' | 'p' ;
-- Q = 'Q' | 'q' ;
-- R = 'R' | 'r' ;
-- S = 'S' | 's' ;
-- T = 'T' | 't' ;
-- U = 'U' | 'u' ;
-- V = 'V' | 'v' ;
-- W = 'W' | 'w' ;
-- X = 'X' | 'x' ;
-- Y = 'Y' | 'y' ;
