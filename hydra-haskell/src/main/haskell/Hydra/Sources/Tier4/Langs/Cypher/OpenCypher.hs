{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Cypher.OpenCypher where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


cypherNs = Namespace "hydra/langs/cypher/openCypher"

cypher = typeref cypherNs

openCypherModule :: Module Kv
openCypherModule = Module cypherNs elements [] [hydraCoreModule] $
    Just "An OpenCypher query model based on the M23 EBNF grammar. See https://opencypher.org/resources."
  where
    def = datatype cypherNs
    elements = [

--Cypher = [SP], Statement, [[SP], ';'], [SP], EOI ;
--
--Statement = Query ;
--
--Query = RegularQuery
--      | StandaloneCall
--      ;
      def "Query" $
        union [
          "regular">: cypher "RegularQuery",
          "standalone">: cypher "StandaloneCall"],

--RegularQuery = SingleQuery, { [SP], Union } ;
      def "RegularQuery" $
        record [
          "query">: cypher "SingleQuery",
          "union">: list $ cypher "Union"],

--Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
--      | ((U,N,I,O,N), [SP], SingleQuery)
--      ;
      def "Union" $
        union [
          "all">: cypher "SingleQuery",
          "distinct">: cypher "SingleQuery"],

--SingleQuery = SinglePartQuery
--            | MultiPartQuery
--            ;
      def "SingleQuery" $
        union [
          "singlePart">: cypher "SinglePartQuery",
          "multiPart">: cypher "MultiPartQuery"],

--SinglePartQuery = ({ ReadingClause, [SP] }, Return)
--                | ({ ReadingClause, [SP] }, UpdatingClause, { [SP], UpdatingClause }, [[SP], Return])
--                ;
      def "SinglePartQuery" $
        union [
          "reading">: cypher "ReadingQuery",
          "return">: cypher "UpdatingQuery"],

      def "ReadingQuery" $
        record [
          "reading">:
            list $ cypher "ReadingClause",
          "return">:
            cypher "ProjectionBody"],

     def "UpdatingQuery" $
        record [
          "reading">:
            list $ cypher "ReadingClause",
          "updating">:
            nonemptyList $ cypher "UpdatingClause",
          "return">:
            optional $ cypher "ProjectionBody"],

--MultiPartQuery = { { ReadingClause, [SP] }, { UpdatingClause, [SP] }, With, [SP] }-, SinglePartQuery ;
      def "MultiPartQuery" $
        record [
          -- TODO: wheck whether order is significant among these reading and updating clauses
          "reading">:
            list $ cypher "ReadingClause",
          "updating">:
            list $ cypher "UpdatingClause",
          "with">:
            cypher "With",
          "query">:
            cypher "SinglePartQuery"],

--UpdatingClause = Create
--               | Merge
--               | Delete
--               | Set
--               | Remove
--               ;
      def "UpdatingClause" $
        union [
          "create">: cypher "Create",
          "merge">: cypher "Merge",
          "delete">: cypher "Delete",
          "set">: list $ cypher "SetItem",
          "remove">: cypher "Remove"],

--ReadingClause = Match
--              | Unwind
--              | InQueryCall
--              ;
      def "ReadingClause" $
        union [
          "match">: cypher "Match",
          "unwind">: cypher "Unwind",
          "call">: cypher "InQueryCall"],

--Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), [SP], Pattern, [[SP], Where] ;
      def "Match" $
        record [
          "optional">: boolean,
          "pattern">: cypher "Pattern"],

      def "Pattern" $
        record [
          "parts">: nonemptyList $ cypher "PatternPart",
          "where">: optional $ cypher "Expression"],

--Unwind = (U,N,W,I,N,D), [SP], Expression, SP, (A,S), SP, Variable ;
      def "Unwind" $
        record [
          "expression">: cypher "Expression",
          "variable">: cypher "Variable"],

--Merge = (M,E,R,G,E), [SP], PatternPart, { SP, MergeAction } ;
      def "Merge" $
        record [
          "pattern">: cypher "PatternPart",
          "actions">: list $ cypher "MergeAction"],

--MergeAction = ((O,N), SP, (M,A,T,C,H), SP, Set)
--            | ((O,N), SP, (C,R,E,A,T,E), SP, Set)
--            ;
      def "MergeAction" $
        record [
          "create">: boolean,
          "set">: nonemptyList $ cypher "SetItem"],

--Create = (C,R,E,A,T,E), [SP], Pattern ;
      def "Create" $ nonemptyList $ cypher "PatternPart",

--Set = (S,E,T), [SP], SetItem, { [SP], ',', [SP], SetItem } ;

--SetItem = (PropertyExpression, [SP], '=', [SP], Expression)
--        | (Variable, [SP], '=', [SP], Expression)
--        | (Variable, [SP], '+=', [SP], Expression)
--        | (Variable, [SP], NodeLabels)
--        ;
      def "SetItem" $
        union [
          "propertyEquals">: cypher "PropertyEquals",
          "variableEquals">: cypher "VariableEquals",
          "variablePlusEquals">: cypher "VariablePlusEquals",
          "variableNodeLabels">: cypher "VariableNodeLabels"],

      def "PropertyEquals" $
        record [
            "left">: cypher "PropertyExpression",
            "right">: cypher "Expression"],

      def "VariableEquals" $
        record [
          "left">: cypher "Variable",
          "right">: cypher "Expression"],

      def "VariablePlusEquals" $
        record [
          "left">: cypher "Variable",
          "right">: cypher "Expression"],

      def "VariableNodeLabels" $
        record [
          "variable">: cypher "Variable",
          "labels">: nonemptyList $ cypher "NodeLabel"],

--
--Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;
      def "Delete" $
        record [
          "detach">: boolean,
          "expressions">: nonemptyList $ cypher "Expression"],

--Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;
      def "Remove" $ nonemptyList $ cypher "RemoveItem",

--RemoveItem = (Variable, NodeLabels)
--           | PropertyExpression
--           ;
      def "RemoveItem" $
        union [
          "variableNodeLabels">: cypher "VariableNodeLabels",
          "propertyExpression">: cypher "PropertyExpression"],

--InQueryCall = (C,A,L,L), SP, ExplicitProcedureInvocation, [[SP], (Y,I,E,L,D), SP, YieldItems] ;
      def "InQueryCall" $
        record [
          "invocation">: cypher "ProcedureInvocation",
          "yield">: optional $ cypher "YieldItems"],

--StandaloneCall = (C,A,L,L), SP, (ExplicitProcedureInvocation | ImplicitProcedureInvocation), [[SP], (Y,I,E,L,D), SP, ('*' | YieldItems)] ;
      def "StandaloneCall" $
        record [
          "invocation">: cypher "ProcedureInvocation",
          "yield">: optional $ cypher "YieldExpression"],

      def "YieldExpression" $
        union [
          "all">: unit,
          "list">: cypher "YieldItems"],

--YieldItems = YieldItem, { [SP], ',', [SP], YieldItem }, [[SP], Where] ;
      def "YieldItems" $
        record [
          "items">: list $ cypher "YieldItem",
          "where">: optional $ cypher "Expression"],

--YieldItem = [ProcedureResultField, SP, (A,S), SP], Variable ;
      def "YieldItem" $
        record [
          "resultField">: optional $ cypher "ProcedureResultField",
          "variable">: cypher "Variable"],

--With = (W,I,T,H), ProjectionBody, [[SP], Where] ;
      def "With" $
        record [
          "projection">:
            cypher "ProjectionBody",
          "where">:
            optional $ cypher "Expression"],

--Return = (R,E,T,U,R,N), ProjectionBody ;

--ProjectionBody = [[SP], (D,I,S,T,I,N,C,T)], SP, ProjectionItems, [SP, Order], [SP, Skip], [SP, Limit] ;
      def "ProjectionBody" $
        record [
          "distinct">: boolean,
          "items">:
            cypher "ProjectionItems",
          "order">:
            list $ cypher "SortItem",
          "skip">:
            optional $ cypher "Expression",
          "limit">:
            optional $ cypher "Expression"],

--ProjectionItems = ('*', { [SP], ',', [SP], ProjectionItem })
--                | (ProjectionItem, { [SP], ',', [SP], ProjectionItem })
--                ;
      def "ProjectionItems" $
        record [
          "all">: boolean,
          "items">: list $ cypher "ProjectionItem"],

--ProjectionItem = (Expression, SP, (A,S), SP, Variable)
--               | Expression
--               ;
      def "ProjectionItem" $
        record [
          "expression">: cypher "Expression",
          "alias">: optional $ cypher "Variable"],

--Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;

--Skip = (S,K,I,P), SP, Expression ;

--Limit = (L,I,M,I,T), SP, Expression ;

--SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
      def "SortItem" $
        record [
          "sortBy">: cypher "Expression",
          "descending">: boolean],

--Where = (W,H,E,R,E), SP, Expression ;

--Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;

--PatternPart = (Variable, [SP], '=', [SP], AnonymousPatternPart)
--            | AnonymousPatternPart
--            ;
      def "PatternPart" $
        record [
          "variable">: optional $ cypher "Variable",
          "pattern">: cypher "PatternElement"],


--AnonymousPatternPart = PatternElement ;

--PatternElement = (NodePattern, { [SP], PatternElementChain })
--               | ('(', PatternElement, ')')
--               ;
      def "PatternElement" $
        union [
          "nodePattern">: cypher "NodePatternExpression",
          "parens">: cypher "PatternElement"],

     def "NodePatternExpression" $
       record [
         "pattern">: cypher "NodePattern",
         "chains">: list $ cypher "PatternElementChain"],

--RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;
      def "RelationshipsPattern" $
        record [
          "pattern">: cypher "NodePattern",
          "chains">: nonemptyList $ cypher "PatternElementChain"],

--NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
      def "NodePattern" $
        record [
          "variable">: optional $ cypher "Variable",
          "labels">: list $ cypher "NodeLabel",
          "properties">: optional $ cypher "Properties"],

--PatternElementChain = RelationshipPattern, [SP], NodePattern ;
      def "PatternElementChain" $
        record [
          "relationship">: cypher "RelationshipPattern",
          "node">: cypher "NodePattern"],

--RelationshipPattern = (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
--                    | (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash)
--                    | (Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
--                    | (Dash, [SP], [RelationshipDetail], [SP], Dash)
--                    ;
      def "RelationshipPattern" $
        record [
          "direction">: cypher "RelationshipDirection",
            "detail">: cypher "RelationshipDetail"],

      def "RelationshipDirection" $
        enum [
          "leftToRight",
          "rightToLeft",
          "bidirectional",
          "undirected"],

--RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;
      def "RelationshipDetail" $
        record [
          "variable">: optional $ cypher "Variable",
          "types">: list $ cypher "RelTypeName",
          "range">: optional $ cypher "RangeLiteral",
          "properties">: optional $ cypher "Properties"],

--Properties = MapLiteral
--           | Parameter
--           ;
      def "Properties" $
        union [
          "map">: cypher "MapLiteral",
          "parameter">: cypher "Parameter"],

--RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;

--NodeLabels = NodeLabel, { [SP], NodeLabel } ;

--NodeLabel = ':', [SP], LabelName ;
      def "NodeLabel" string,

--RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;
      def "RangeLiteral" $
        record [
          "from">: optional int32,
          "to">: optional int32],

--LabelName = SchemaName ;

--RelTypeName = SchemaName ;
      def "RelTypeName" string,

--PropertyExpression = Atom, { [SP], PropertyLookup }- ;
      def "PropertyExpression" $
        record [
          "atom">: cypher "Atom",
          "lookups">: nonemptyList $ cypher "PropertyKeyName"],

--Expression = OrExpression ;
      def "Expression" $ cypher "OrExpression",

--OrExpression = XorExpression, { SP, (O,R), SP, XorExpression } ;
      def "OrExpression" $ nonemptyList $ cypher "XorExpression",

--XorExpression = AndExpression, { SP, (X,O,R), SP, AndExpression } ;
      def "XorExpression" $ nonemptyList $ cypher "AndExpression",

--AndExpression = NotExpression, { SP, (A,N,D), SP, NotExpression } ;
      def "AndExpression" $ nonemptyList $ cypher "NotExpression",

--NotExpression = { (N,O,T), [SP] }, ComparisonExpression ;
      def "NotExpression" $
        record [
          "not">: boolean,
          "expression">: cypher "ComparisonExpression"],

--ComparisonExpression = StringListNullPredicateExpression, { [SP], PartialComparisonExpression } ;
      def "ComparisonExpression" $
        record [
          "primary">: cypher "StringListNullPredicateExpression",
          "comparisons">: list $ cypher "PartialComparisonExpression"],

--PartialComparisonExpression = ('=', [SP], StringListNullPredicateExpression)
--                            | ('<>', [SP], StringListNullPredicateExpression)
--                            | ('<', [SP], StringListNullPredicateExpression)
--                            | ('>', [SP], StringListNullPredicateExpression)
--                            | ('<=', [SP], StringListNullPredicateExpression)
--                            | ('>=', [SP], StringListNullPredicateExpression)
--                            ;
      def "PartialComparisonExpression" $
        record [
            "operator">: cypher "ComparisonOperator",
            "expression">: cypher "StringListNullPredicateExpression"],

      def "ComparisonOperator" $
        enum ["equal", "notEqual", "lessThan", "greaterThan", "lessThanOrEqual", "greaterThanOrEqual"],

--StringListNullPredicateExpression = AddOrSubtractExpression, { StringPredicateExpression | ListPredicateExpression | NullPredicateExpression } ;
      def "StringListNullPredicateExpression" $
        record [
          "prefix">: cypher "AddOrSubtractExpression",
          "suffixes">: list $ cypher "StringListNullPredicateSuffix"],

      def "StringListNullPredicateSuffix" $
        union [
          "string">: cypher "StringPredicateExpression",
          "list">: cypher "AddOrSubtractExpression",
          "null">:
            doc "True if NULL, false if NOT NULL" $
            boolean],

--StringPredicateExpression = ((SP, (S,T,A,R,T,S), SP, (W,I,T,H)) | (SP, (E,N,D,S), SP, (W,I,T,H)) | (SP, (C,O,N,T,A,I,N,S))), [SP], AddOrSubtractExpression ;
      def "StringPredicateExpression" $
        record [
          "predicate">: cypher "StringPredicate",
          "expression">: cypher "AddOrSubtractExpression"],

      def "StringPredicate" $
        enum ["startsWith", "endsWith", "contains"],

--ListPredicateExpression = SP, (I,N), [SP], AddOrSubtractExpression ;

--NullPredicateExpression = (SP, (I,S), SP, (N,U,L,L))
--                        | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L))
--                        ;

--AddOrSubtractExpression = MultiplyDivideModuloExpression, { ([SP], '+', [SP], MultiplyDivideModuloExpression) | ([SP], '-', [SP], MultiplyDivideModuloExpression) } ;
     def "AddOrSubtractExpression" $
       record [
         "lhs">: cypher "MultiplyDivideModuloExpression",
         "rhs">: list $  cypher "AddOrSubtractRhs"],
     
     def "AddOrSubtractRhs" $
       record [
         "operator">: cypher "PlusOrMinus",
          "expression">: cypher "MultiplyDivideModuloExpression"],

--MultiplyDivideModuloExpression = PowerOfExpression, { ([SP], '*', [SP], PowerOfExpression) | ([SP], '/', [SP], PowerOfExpression) | ([SP], '%', [SP], PowerOfExpression) } ;
      def "MultiplyDivideModuloExpression" $
        record [
          "lhs">: cypher "PowerOfExpression",
          "rhs">: list $  cypher "MultiplyDivideModuloRhs"],

      def "MultiplyDivideModuloRhs" $
        record [
          "operator">: cypher "TimesOrDivideOrModulo",
          "expression">: cypher "PowerOfExpression"],

      def "TimesOrDivideOrModulo" $
        enum ["times", "divide", "modulo"],

--PowerOfExpression = UnaryAddOrSubtractExpression, { [SP], '^', [SP], UnaryAddOrSubtractExpression } ;
      def "PowerOfExpression" $ nonemptyList $ cypher "UnaryAddOrSubtractExpression",

--UnaryAddOrSubtractExpression = NonArithmeticOperatorExpression
--                             | (('+' | '-'), [SP], NonArithmeticOperatorExpression)
--                             ;
      def "UnaryAddOrSubtractExpression" $
        record [
          "operator">: optional $ cypher "PlusOrMinus",
          "expression">: cypher "NonArithmeticOperatorExpression"],

      def "PlusOrMinus" $
        enum ["plus", "minus"],

--NonArithmeticOperatorExpression = Atom, { ([SP], ListOperatorExpression) | ([SP], PropertyLookup) }, [[SP], NodeLabels] ;
      def "NonArithmeticOperatorExpression" $
        record [
          "atom">: cypher "Atom",
          "suffixes">: list $ cypher "NonArithmeticOperatorInfix",
          "labels">: list $ cypher "NodeLabel"],

      def "NonArithmeticOperatorInfix" $
        union [
          "list">: cypher "ListOperatorExpression",
          "property">: cypher "PropertyKeyName"],

--ListOperatorExpression = ('[', Expression, ']')
--                       | ('[', [Expression], '..', [Expression], ']')
--                       ;
      def "ListOperatorExpression" $
        union [
          "single">: cypher "Expression",
          "range">: cypher "ListOperatorRange"],

      def "ListOperatorRange" $
        record [
          "from">: optional $ cypher "Expression",
          "to">: optional $ cypher "Expression"],

--PropertyLookup = '.', [SP], (PropertyKeyName) ;

--Atom = Literal
--     | Parameter
--     | CaseExpression
--     | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
--     | ListComprehension
--     | PatternComprehension
--     | Quantifier
--     | PatternPredicate
--     | ParenthesizedExpression
--     | FunctionInvocation
--     | ExistentialSubquery
--     | Variable
--     ;
      def "Atom" $
        union [
          "parameter">: cypher "Parameter",
          "case">: cypher "CaseExpression",
          "count">: unit,
          "list">: cypher "ListComprehension",
          "pattern">: cypher "PatternComprehension",
          "quantifier">: cypher "Quantifier",
          "predicate">: cypher "RelationshipsPattern",
          "parens">: cypher "Expression",
          "function">: cypher "FunctionInvocation",
          "existence">: cypher "ExistentialSubquery",
          "variable">: cypher "Variable"],

--CaseExpression = (((C,A,S,E), { [SP], CaseAlternative }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternative }-)), [[SP], (E,L,S,E), [SP], Expression], [SP], (E,N,D) ;
      def "CaseExpression" $
        union [
          "cases">: nonemptyList $ cypher "Case",
          "else">: optional $ cypher "Expression"],

      def "Case" $
        record [
          "expression">: optional $ cypher "Expression",
          "alternatives">: nonemptyList $ cypher "CaseAlternative"],

--CaseAlternative = (W,H,E,N), [SP], Expression, [SP], (T,H,E,N), [SP], Expression ;
      def "CaseAlternative" $
        record [
          "when">: cypher "Expression",
          "then">: cypher "Expression"],

--ListComprehension = '[', [SP], FilterExpression, [[SP], '|', [SP], Expression], [SP], ']' ;
      def "ListComprehension" $
        record [
          "head">: cypher "FilterExpression",
          "body">: optional $ cypher "Expression"],

--PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [Where, [SP]], '|', [SP], Expression, [SP], ']' ;
      def "PatternComprehension" $
        record [
          "variable">: optional $ cypher "Variable",
          "pattern">: cypher "RelationshipsPattern",
          "where">: optional $ cypher "Expression",
          "body">: cypher "Expression"],

--Quantifier = ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
--           | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
--           | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
--           | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
--           ;
      def "Quantifier" $
        record [
          "type">: cypher "QuantifierType",
          "expression">: cypher "FilterExpression"],

      def "QuantifierType" $
        enum ["all", "any", "none", "single"],

--FilterExpression = IdInColl, [[SP], Where] ;
      def "FilterExpression" $
        record [
          "id">: cypher "IdInColl",
          "where">: optional $ cypher "Expression"],

--PatternPredicate = RelationshipsPattern ;

--ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;

--IdInColl = Variable, SP, (I,N), SP, Expression ;
      def "IdInColl" $
        record [
          "variable">: cypher "Variable",
          "expression">: cypher "Expression"],

--FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;
      def "FunctionInvocation" $
        record [
          "function">: cypher "FunctionName",
          "distinct">: boolean,
          "arguments">: list $ cypher "Expression"],

--FunctionName = Namespace, SymbolicName ;
      def "FunctionName" $
        record [
          "namespace">: cypher "Namespace",
          "name">: string],

--ExistentialSubquery = (E,X,I,S,T,S), [SP], '{', [SP], (RegularQuery | (Pattern, [[SP], Where])), [SP], '}' ;
      def "ExistentialSubquery" $
        union [
          "query">: cypher "RegularQuery",
          "pattern">: cypher "Pattern"],

--ExplicitProcedureInvocation = ProcedureName, [SP], '(', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

--ImplicitProcedureInvocation = ProcedureName ;
      def "ProcedureInvocation" $
        record [
          "name">: cypher "ProcedureName",
          "arguments">: optional $ list $ cypher "Expression"],

--ProcedureResultField = SymbolicName ;
      def "ProcedureResultField" string,

--ProcedureName = Namespace, SymbolicName ;
       def "ProcedureName" $
          record [
             "namespace">: cypher "Namespace",
             "name">: string],

--Namespace = { SymbolicName, '.' } ;
      def "Namespace" $ list string,

--Variable = SymbolicName ;
      def "Variable" string,

--Literal = BooleanLiteral
--        | (N,U,L,L)
--        | NumberLiteral
--        | StringLiteral
--        | ListLiteral
--        | MapLiteral
--        ;
      def "Literal" $
        union [
          "boolean">: boolean,
          "null">: unit,
          "double">: float64,
          "integer">: int32,
          "string">: string,
          "list">: list $ cypher "Expression",
          "map">: cypher "MapLiteral"],

--BooleanLiteral = (T,R,U,E)
--               | (F,A,L,S,E)
--               ;
--
--NumberLiteral = DoubleLiteral
--              | IntegerLiteral
--              ;
--
--IntegerLiteral = HexInteger
--               | OctalInteger
--               | DecimalInteger
--               ;
--
--HexInteger = '0x', { HexDigit }- ;
--
--DecimalInteger = ZeroDigit
--               | (NonZeroDigit, { Digit })
--               ;
--
--OctalInteger = '0o', { OctDigit }- ;
--
--HexLetter = (A)
--          | (B)
--          | (C)
--          | (D)
--          | (E)
--          | (F)
--          ;
--
--HexDigit = Digit
--         | HexLetter
--         ;
--
--Digit = ZeroDigit
--      | NonZeroDigit
--      ;
--
--NonZeroDigit = NonZeroOctDigit
--             | '8'
--             | '9'
--             ;
--
--NonZeroOctDigit = '1'
--                | '2'
--                | '3'
--                | '4'
--                | '5'
--                | '6'
--                | '7'
--                ;
--
--OctDigit = ZeroDigit
--         | NonZeroOctDigit
--         ;
--
--ZeroDigit = '0' ;
--
--DoubleLiteral = ExponentDecimalReal
--              | RegularDecimalReal
--              ;
--
--ExponentDecimalReal = ({ Digit }- | ({ Digit }-, '.', { Digit }-) | ('.', { Digit }-)), (E), ['-'], { Digit }- ;
--
--RegularDecimalReal = { Digit }, '.', { Digit }- ;
--
--StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
--              | ("'", { ANY - ("'" | '\') | EscapedChar }, "'")
--              ;
--
--EscapedChar = '\', ('\' | "'" | '"' | (B) | (F) | (N) | (R) | (T) | ((U), 4 * HexDigit) | ((U), 8 * HexDigit)) ;
--
--ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;

--MapLiteral = '{', [SP], [PropertyKeyName, [SP], ':', [SP], Expression, [SP], { ',', [SP], PropertyKeyName, [SP], ':', [SP], Expression, [SP] }], '}' ;
      def "MapLiteral" $
        Types.map (cypher "PropertyKeyName") (cypher "Expression"),

--PropertyKeyName = SchemaName ;
      def "PropertyKeyName" string,

--Parameter = '$', (SymbolicName | DecimalInteger) ;
        def "Parameter" $
          record [
            "name">: string,
            "index">: int32]]

-- Note: the following terminal symbols are part of the EBNF grammar but are not needed in the model

--SchemaName = SymbolicName
--           | ReservedWord
--           ;

--ReservedWord = (A,L,L)
--             | (A,S,C)
--             | (A,S,C,E,N,D,I,N,G)
--             | (B,Y)
--             | (C,R,E,A,T,E)
--             | (D,E,L,E,T,E)
--             | (D,E,S,C)
--             | (D,E,S,C,E,N,D,I,N,G)
--             | (D,E,T,A,C,H)
--             | (E,X,I,S,T,S)
--             | (L,I,M,I,T)
--             | (M,A,T,C,H)
--             | (M,E,R,G,E)
--             | (O,N)
--             | (O,P,T,I,O,N,A,L)
--             | (O,R,D,E,R)
--             | (R,E,M,O,V,E)
--             | (R,E,T,U,R,N)
--             | (S,E,T)
--             | (S,K,I,P)
--             | (W,H,E,R,E)
--             | (W,I,T,H)
--             | (U,N,I,O,N)
--             | (U,N,W,I,N,D)
--             | (A,N,D)
--             | (A,S)
--             | (C,O,N,T,A,I,N,S)
--             | (D,I,S,T,I,N,C,T)
--             | (E,N,D,S)
--             | (I,N)
--             | (I,S)
--             | (N,O,T)
--             | (O,R)
--             | (S,T,A,R,T,S)
--             | (X,O,R)
--             | (F,A,L,S,E)
--             | (T,R,U,E)
--             | (N,U,L,L)
--             | (C,O,N,S,T,R,A,I,N,T)
--             | (D,O)
--             | (F,O,R)
--             | (R,E,Q,U,I,R,E)
--             | (U,N,I,Q,U,E)
--             | (C,A,S,E)
--             | (W,H,E,N)
--             | (T,H,E,N)
--             | (E,L,S,E)
--             | (E,N,D)
--             | (M,A,N,D,A,T,O,R,Y)
--             | (S,C,A,L,A,R)
--             | (O,F)
--             | (A,D,D)
--             | (D,R,O,P)
--             ;

--SymbolicName = UnescapedSymbolicName
--             | EscapedSymbolicName
--             | HexLetter
--             | (C,O,U,N,T)
--             | (F,I,L,T,E,R)
--             | (E,X,T,R,A,C,T)
--             | (A,N,Y)
--             | (N,O,N,E)
--             | (S,I,N,G,L,E)
--             ;

--UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;

--(* Based on the unicode identifier and pattern syntax
-- *   (http://www.unicode.org/reports/tr31/)
-- * And extended with a few characters.
-- *)IdentifierStart = ID_Start
--                | Pc
--                ;
--
--(* Based on the unicode identifier and pattern syntax
-- *   (http://www.unicode.org/reports/tr31/)
-- * And extended with a few characters.
-- *)IdentifierPart = ID_Continue
--               | Sc
--               ;
--
--(* Any character except "`", enclosed within `backticks`. Backticks are escaped with double backticks.
-- *)EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
--
--SP = { whitespace }- ;
--
--whitespace = SPACE
--           | TAB
--           | LF
--           | VT
--           | FF
--           | CR
--           | FS
--           | GS
--           | RS
--           | US
--           | ' '
--           | '᠎'
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | ' '
--           | '　'
--           | ' '
--           | ' '
--           | ' '
--           | Comment
--           ;
--
--Comment = ('/*', { ANY - ('*') | ('*', ANY - ('/')) }, '*/')
--        | ('//', { ANY - (LF | CR) }, [CR], (LF | EOI))
--        ;
--
--LeftArrowHead = '<'
--              | '⟨'
--              | '〈'
--              | '﹤'
--              | '＜'
--              ;
--
--RightArrowHead = '>'
--               | '⟩'
--               | '〉'
--               | '﹥'
--               | '＞'
--               ;
--
--Dash = '-'
--     | '­'
--     | '‐'
--     | '‑'
--     | '‒'
--     | '–'
--     | '—'
--     | '―'
--     | '−'
--     | '﹘'
--     | '﹣'
--     | '－'
--     ;
--
--A = 'A' | 'a' ;
--
--B = 'B' | 'b' ;
--
--C = 'C' | 'c' ;
--
--D = 'D' | 'd' ;
--
--E = 'E' | 'e' ;
--
--F = 'F' | 'f' ;
--
--G = 'G' | 'g' ;
--
--H = 'H' | 'h' ;
--
--I = 'I' | 'i' ;
--
--K = 'K' | 'k' ;
--
--L = 'L' | 'l' ;
--
--M = 'M' | 'm' ;
--
--N = 'N' | 'n' ;
--
--O = 'O' | 'o' ;
--
--P = 'P' | 'p' ;
--
--Q = 'Q' | 'q' ;
--
--R = 'R' | 'r' ;
--
--S = 'S' | 's' ;
--
--T = 'T' | 't' ;
--
--U = 'U' | 'u' ;
--
--V = 'V' | 'v' ;
--
--W = 'W' | 'w' ;
--
--X = 'X' | 'x' ;
--
--Y = 'Y' | 'y' ;
