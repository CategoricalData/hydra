module Hydra.Ext.Sources.Cypher.OpenCypher where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.cypher.openCypher"

define :: String -> Type -> Binding
define = defineType ns

cypher :: String -> Type
cypher = typeref ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just ("A Cypher model based on the OpenCypher specification (version 23), copyright Neo Technology, available at:\n" ++
      "  https://opencypher.org/resources/")
  where
    elements = [
      query,
      regularQuery,
      union_,
      singleQuery,
      singlePartQuery,
      withClause,
      multiPartQuery,
      updatingClause,
      readingClause,
      match,
      unwind,
      merge,
      matchOrCreate,
      mergeAction,
      create,
      set,
      setItem,
      propertyEquals,
      variableEquals,
      variablePlusEquals,
      variableAndNodeLabels,
      delete,
      remove,
      removeItem,
      inQueryCall,
      procedureInvocation,
      starOrYieldItems,
      standaloneCall,
      yieldItems,
      yieldItem,
      with,
      return_,
      projectionBody,
      projectionItems,
      projectionItem,
      order,
      skip,
      limit,
      sortOrder,
      sortItem,
      where_,
      pattern,
      patternPart,
      anonymousPatternPart,
      nodePatternChain,
      patternElement,
      relationshipsPattern,
      nodePattern,
      patternElementChain,
      relationshipPattern,
      relationshipDetail,
      properties,
      relationshipTypes,
      nodeLabels,
      nodeLabel,
      rangeLiteral,
      relTypeName,
      propertyExpression,
      expression,
      orExpression,
      xorExpression,
      andExpression,
      notExpression,
      comparisonExpression,
      comparisonOperator,
      partialComparisonExpression,
      stringListNullPredicateExpression,
      stringListNullPredicateRightHandSide,
      stringPredicateExpression,
      stringPredicateOperator,
      listPredicateExpression,
      nullPredicateExpression,
      addOrSubtractExpression,
      addOrSubtractRightHandSide,
      addOrSubtractOperator,
      multiplyDivideModuloExpression,
      multiplyDivideModuloRightHandSide,
      multiplyDivideModuloOperator,
      powerOfExpression,
      unaryAddOrSubtractExpression,
      listOperatorExpressionOrPropertyLookup,
      nonArithmeticOperatorExpression,
      rangeExpression,
      listOperatorExpression,
      propertyLookup,
      atom,
      caseExpression,
      caseAlternative,
      listComprehension,
      patternComprehension,
      quantifier,
      quantifierOperator,
      filterExpression,
      patternPredicate,
      parenthesizedExpression,
      idInColl,
      functionInvocation,
      qualifiedName,
      patternWhere,
      existentialSubquery,
      explicitProcedureInvocation,
      implicitProcedureInvocation,
      procedureResultField,
      variable,
      literal,
      numberLiteral,
      stringLiteral,
      listLiteral,
      mapLiteral,
      keyValuePair,
      propertyKeyName,
      parameter]

-- Cypher = [SP], Statement, [[SP], ';'], [SP], EOI ;
--
-- Statement = Query ;
--
-- Query = RegularQuery
--       | StandaloneCall
--       ;

query :: Binding
query = define "Query" $
  T.union [
    "regular">: cypher "RegularQuery",
    "standalone">: cypher "StandaloneCall"]

-- RegularQuery = SingleQuery, { [SP], Union } ;

regularQuery :: Binding
regularQuery = define "RegularQuery" $
  T.record [
    "head">: cypher "SingleQuery",
    "rest">: T.list $ cypher "Union"]

-- Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
--       | ((U,N,I,O,N), [SP], SingleQuery)
--       ;

union_ :: Binding
union_ = define "Union" $
  T.record [
    "all">: T.boolean,
    "query">: cypher "SingleQuery"]

-- SingleQuery = SinglePartQuery
--             | MultiPartQuery
--             ;
singleQuery :: Binding
singleQuery = define "SingleQuery" $
  T.union [
    "singlePart">: cypher "SinglePartQuery",
    "multiPart">: cypher "MultiPartQuery"]

-- SinglePartQuery = ({ ReadingClause, [SP] }, Return)
--                 | ({ ReadingClause, [SP] }, UpdatingClause, { [SP], UpdatingClause }, [[SP], Return])
--                 ;

singlePartQuery :: Binding
singlePartQuery = define "SinglePartQuery" $
  T.record [
    "reading">: T.list $ cypher "ReadingClause",
    "updating">: T.list $ cypher "UpdatingClause",
    "return">: T.maybe $ cypher "Return"]

-- MultiPartQuery = { { ReadingClause, [SP] }, { UpdatingClause, [SP] }, With, [SP] }-, SinglePartQuery ;

withClause :: Binding
withClause = define "WithClause" $
  T.record [
    "reading">: T.list $ cypher "ReadingClause",
    "updating">: T.list $ cypher "UpdatingClause",
    "with">: cypher "With"]

multiPartQuery :: Binding
multiPartQuery = define "MultiPartQuery" $
  T.record [
    "with">: T.list $ cypher "WithClause",
    "body">: cypher "SinglePartQuery"]

-- UpdatingClause = Create
--                | Merge
--                | Delete
--                | Set
--                | Remove
--                ;

updatingClause :: Binding
updatingClause = define "UpdatingClause" $
  T.union [
    "create">: cypher "Create",
    "merge">: cypher "Merge",
    "delete">: cypher "Delete",
    "set">: cypher "Set",
    "remove">: cypher "Remove"]

-- ReadingClause = Match
--               | Unwind
--               | InQueryCall
--               ;

readingClause :: Binding
readingClause = define "ReadingClause" $
  T.union [
    "match">: cypher "Match",
    "unwind">: cypher "Unwind",
    "inQueryCall">: cypher "InQueryCall"]

-- Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), [SP], Pattern, [[SP], Where] ;

match :: Binding
match = define "Match" $
  T.record [
    "optional">: T.boolean,
    "pattern">: cypher "Pattern",
    "where">: T.maybe $ cypher "Where"]

-- Unwind = (U,N,W,I,N,D), [SP], Expression, SP, (A,S), SP, Variable ;

unwind :: Binding
unwind = define "Unwind" $
  T.record [
    "expression">: cypher "Expression",
    "variable">: cypher "Variable"]

-- Merge = (M,E,R,G,E), [SP], PatternPart, { SP, MergeAction } ;

merge :: Binding
merge = define "Merge" $
  T.record [
    "patternPart">: cypher "PatternPart",
    "actions">: T.list $ cypher "MergeAction"]

-- MergeAction = ((O,N), SP, (M,A,T,C,H), SP, Set)
--             | ((O,N), SP, (C,R,E,A,T,E), SP, Set)
--             ;

matchOrCreate :: Binding
matchOrCreate = define "MatchOrCreate" $
  T.enum ["match", "create"]

mergeAction :: Binding
mergeAction = define "MergeAction" $
  T.record [
    "action">: cypher "MatchOrCreate",
    "set">: cypher "Set"]

-- Create = (C,R,E,A,T,E), [SP], Pattern ;

create :: Binding
create = define "Create" $
  T.wrap $ cypher "Pattern"

-- Set = (S,E,T), [SP], SetItem, { [SP], ',', [SP], SetItem } ;

set :: Binding
set = define "Set" $
  T.wrap $ T.list $ cypher "SetItem"

-- SetItem = (PropertyExpression, [SP], '=', [SP], Expression)
--         | (Variable, [SP], '=', [SP], Expression)
--         | (Variable, [SP], '+=', [SP], Expression)
--         | (Variable, [SP], NodeLabels)
--         ;

setItem :: Binding
setItem = define "SetItem" $
  T.union [
    "property">: cypher "PropertyEquals",
    "variableEqual">: cypher "VariableEquals",
    "variablePlusEqual">: cypher "VariablePlusEquals",
    "variableLabels">: cypher "VariableAndNodeLabels"]

propertyEquals :: Binding
propertyEquals = define "PropertyEquals" $
  T.record [
    "lhs">: cypher "PropertyExpression",
    "rhs">: cypher "Expression"]

variableEquals :: Binding
variableEquals = define "VariableEquals" $
  T.record [
    "lhs">: cypher "Variable",
    "rhs">: cypher "Expression"]

variablePlusEquals :: Binding
variablePlusEquals = define "VariablePlusEquals" $
  T.record [
    "lhs">: cypher "Variable",
    "rhs">: cypher "Expression"]

variableAndNodeLabels :: Binding
variableAndNodeLabels = define "VariableAndNodeLabels" $
  T.record [
    "variable">: cypher "Variable",
    "labels">: cypher "NodeLabels"]

-- Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;

delete :: Binding
delete = define "Delete" $
  T.record [
    "detach">: T.boolean,
    "expressions">: T.list $ cypher "Expression"]

-- Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;

remove :: Binding
remove = define "Remove" $
  T.wrap $ T.list $ cypher "RemoveItem"

-- RemoveItem = (Variable, NodeLabels)
--            | PropertyExpression
--            ;

removeItem :: Binding
removeItem = define "RemoveItem" $
  T.union [
    "variableLabels">: cypher "VariableAndNodeLabels",
    "property">: cypher "PropertyExpression"]

-- InQueryCall = (C,A,L,L), SP, ExplicitProcedureInvocation, [[SP], (Y,I,E,L,D), SP, YieldItems] ;

inQueryCall :: Binding
inQueryCall = define "InQueryCall" $
  T.record [
    "call">: cypher "ExplicitProcedureInvocation",
    "yieldItems">: T.maybe $ cypher "YieldItems"]

-- StandaloneCall = (C,A,L,L), SP, (ExplicitProcedureInvocation | ImplicitProcedureInvocation), [[SP], (Y,I,E,L,D), SP, ('*' | YieldItems)] ;

procedureInvocation :: Binding
procedureInvocation = define "ProcedureInvocation" $
  T.union [
    "explicit">: cypher "ExplicitProcedureInvocation",
    "implicit">: cypher "ImplicitProcedureInvocation"]

starOrYieldItems :: Binding
starOrYieldItems = define "StarOrYieldItems" $
  T.union [
    "star">: T.unit,
    "items">: cypher "YieldItems"]

standaloneCall :: Binding
standaloneCall = define "StandaloneCall" $
  T.record [
    "call">: cypher "ProcedureInvocation",
    "yieldItems">: T.maybe $ cypher "StarOrYieldItems"]

-- YieldItems = YieldItem, { [SP], ',', [SP], YieldItem }, [[SP], Where] ;

yieldItems :: Binding
yieldItems = define "YieldItems" $
  T.record [
    "items">: T.list $ cypher "YieldItem",
    "where">: T.maybe $ cypher "Where"]

-- YieldItem = [ProcedureResultField, SP, (A,S), SP], Variable ;

yieldItem :: Binding
yieldItem = define "YieldItem" $
  T.record [
    "field">: T.maybe $ cypher "ProcedureResultField",
    "variable">: cypher "Variable"]

-- With = (W,I,T,H), ProjectionBody, [[SP], Where] ;

with :: Binding
with = define "With" $
  T.record [
    "projection">: cypher "ProjectionBody",
    "where">: T.maybe $ cypher "Where"]

-- Return = (R,E,T,U,R,N), ProjectionBody ;

return_ :: Binding
return_ = define "Return" $
  T.wrap $ cypher "ProjectionBody"

-- ProjectionBody = [[SP], (D,I,S,T,I,N,C,T)], SP, ProjectionItems, [SP, Order], [SP, Skip], [SP, Limit] ;

projectionBody :: Binding
projectionBody = define "ProjectionBody" $
  T.record [
    "distinct">: T.boolean,
    "projectionItems">: cypher "ProjectionItems",
    "order">: T.maybe $ cypher "Order",
    "skip">: T.maybe $ cypher "Skip",
    "limit">: T.maybe $ cypher "Limit"]

-- ProjectionItems = ('*', { [SP], ',', [SP], ProjectionItem })
--                 | (ProjectionItem, { [SP], ',', [SP], ProjectionItem })
--                 ;

projectionItems :: Binding
projectionItems = define "ProjectionItems" $
  T.record [
    "star">: T.boolean,
    "explicit">: T.list $ cypher "ProjectionItem"]

-- ProjectionItem = (Expression, SP, (A,S), SP, Variable)
--                | Expression
--                ;

projectionItem :: Binding
projectionItem = define "ProjectionItem" $
    T.record [
      "expression">: cypher "Expression",
      "variable">: T.maybe $ cypher "Variable"]

-- Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;

order :: Binding
order = define "Order" $
  T.wrap $ T.list $ cypher "SortItem"

-- Skip = (S,K,I,P), SP, Expression ;

skip :: Binding
skip = define "Skip" $
  T.wrap $ cypher "Expression"

-- Limit = (L,I,M,I,T), SP, Expression ;

limit :: Binding
limit = define "Limit" $
  T.wrap $ cypher "Expression"

-- SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;

sortOrder :: Binding
sortOrder = define "SortOrder" $
  T.enum ["ascending", "descending"]

sortItem :: Binding
sortItem = define "SortItem" $
  T.record [
    "expression">: cypher "Expression",
    "order">: T.maybe $ cypher "SortOrder"]

-- Where = (W,H,E,R,E), SP, Expression ;

where_ :: Binding
where_ = define "Where" $
  T.wrap $ cypher "Expression"

-- Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;

pattern :: Binding
pattern = define "Pattern" $
  T.wrap $ T.list $ cypher "PatternPart"

-- PatternPart = (Variable, [SP], '=', [SP], AnonymousPatternPart)
--             | AnonymousPatternPart
--             ;

patternPart :: Binding
patternPart = define "PatternPart" $
  T.record [
    "variable">: T.maybe $ cypher "Variable",
    "pattern">: cypher "AnonymousPatternPart"]

-- AnonymousPatternPart = PatternElement ;

anonymousPatternPart :: Binding
anonymousPatternPart = define "AnonymousPatternPart" $
    T.wrap $ cypher "PatternElement"

-- PatternElement = (NodePattern, { [SP], PatternElementChain })
--                | ('(', PatternElement, ')')
--                ;

nodePatternChain :: Binding
nodePatternChain = define "NodePatternChain" $
  T.record [
    "nodePattern">: cypher "NodePattern",
    "chain">: T.list $ cypher "PatternElementChain"]

patternElement :: Binding
patternElement = define "PatternElement" $
  T.union [
    "chained">: cypher "NodePatternChain",
    "parenthesized">: cypher "PatternElement"]

-- RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;

relationshipsPattern :: Binding
relationshipsPattern = define "RelationshipsPattern" $
  T.record [
    "nodePattern">: cypher "NodePattern",
    "chain">: T.list $ cypher "PatternElementChain"]

-- NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
nodePattern :: Binding
nodePattern = define "NodePattern" $
  T.record [
    "variable">: T.maybe $ cypher "Variable",
    "labels">: T.maybe $ cypher "NodeLabels",
    "properties">: T.maybe $ cypher "Properties"]

-- PatternElementChain = RelationshipPattern, [SP], NodePattern ;

patternElementChain :: Binding
patternElementChain = define "PatternElementChain" $
    T.record [
      "relationship">: cypher "RelationshipPattern",
      "node">: cypher "NodePattern"]

-- RelationshipPattern = (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
--                     | (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash)
--                     | (Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
--                     | (Dash, [SP], [RelationshipDetail], [SP], Dash)
--                     ;

relationshipPattern :: Binding
relationshipPattern = define "RelationshipPattern" $
    T.record [
      "leftArrow">: T.boolean,
      "detail">: T.maybe $ cypher "RelationshipDetail",
      "rightArrow">: T.boolean]

-- RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;

relationshipDetail :: Binding
relationshipDetail = define "RelationshipDetail" $
  T.record [
    "variable">: T.maybe $ cypher "Variable",
    "types">: T.maybe $ cypher "RelationshipTypes",
    "range">: T.maybe $ cypher "RangeLiteral",
    "properties">: T.maybe $ cypher "Properties"]

-- Properties = MapLiteral
--            | Parameter
--            ;

properties :: Binding
properties = define "Properties" $
  T.union [
    "map">: cypher "MapLiteral",
    "parameter">: cypher "Parameter"]

-- RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;

-- TODO: check whether the slight difference in colon syntax is significant
relationshipTypes :: Binding
relationshipTypes = define "RelationshipTypes" $
  T.wrap $ T.list $ cypher "RelTypeName"

-- NodeLabels = NodeLabel, { [SP], NodeLabel } ;

nodeLabels :: Binding
nodeLabels = define "NodeLabels" $
  T.wrap $ T.list $ cypher "NodeLabel"

-- NodeLabel = ':', [SP], LabelName ;

nodeLabel :: Binding
nodeLabel = define "NodeLabel" $
  T.wrap T.string

-- RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;

rangeLiteral :: Binding
rangeLiteral = define "RangeLiteral" $
  T.record [
    "start">: T.maybe T.bigint,
    "end">: T.maybe T.bigint]

-- LabelName = SchemaName ;
--
-- RelTypeName = SchemaName ;

relTypeName :: Binding
relTypeName = define "RelTypeName" $
  T.wrap T.string

-- PropertyExpression = Atom, { [SP], PropertyLookup }- ;

propertyExpression :: Binding
propertyExpression = define "PropertyExpression" $
  T.record [
    "atom">: cypher "Atom",
    "lookups">: T.list $ cypher "PropertyLookup"]

-- Expression = OrExpression ;

expression :: Binding
expression = define "Expression" $
  T.wrap $ cypher "OrExpression"

-- OrExpression = XorExpression, { SP, (O,R), SP, XorExpression } ;

orExpression :: Binding
orExpression = define "OrExpression" $
  T.wrap $ T.list $ cypher "XorExpression"

-- XorExpression = AndExpression, { SP, (X,O,R), SP, AndExpression } ;

xorExpression :: Binding
xorExpression = define "XorExpression" $
  T.wrap $ T.list $ cypher "AndExpression"

-- AndExpression = NotExpression, { SP, (A,N,D), SP, NotExpression } ;

andExpression :: Binding
andExpression = define "AndExpression" $
  T.wrap $ T.list $ cypher "NotExpression"

-- NotExpression = { (N,O,T), [SP] }, ComparisonExpression ;

notExpression :: Binding
notExpression = define "NotExpression" $
  T.record [
    "not">: T.boolean,
    "expression">: cypher "ComparisonExpression"]

-- ComparisonExpression = StringListNullPredicateExpression, { [SP], PartialComparisonExpression } ;

comparisonExpression :: Binding
comparisonExpression = define "ComparisonExpression" $
  T.record [
    "left">: cypher "StringListNullPredicateExpression",
    "right">: T.list $ cypher "PartialComparisonExpression"]

-- PartialComparisonExpression = ('=', [SP], StringListNullPredicateExpression)
--                             | ('<>', [SP], StringListNullPredicateExpression)
--                             | ('<', [SP], StringListNullPredicateExpression)
--                             | ('>', [SP], StringListNullPredicateExpression)
--                             | ('<=', [SP], StringListNullPredicateExpression)
--                             | ('>=', [SP], StringListNullPredicateExpression)
--                             ;

comparisonOperator :: Binding
comparisonOperator = define "ComparisonOperator" $
  T.enum [
    "eq",
    "neq",
    "lt",
    "gt",
    "lte",
    "gte"]

partialComparisonExpression :: Binding
partialComparisonExpression = define "PartialComparisonExpression" $
  T.record [
    "operator">: cypher "ComparisonOperator",
    "right">: cypher "StringListNullPredicateExpression"]

-- StringListNullPredicateExpression = AddOrSubtractExpression, { StringPredicateExpression | ListPredicateExpression | NullPredicateExpression } ;

stringListNullPredicateExpression :: Binding
stringListNullPredicateExpression = define "StringListNullPredicateExpression" $
  T.record [
    "left">: cypher "AddOrSubtractExpression",
    "right">: T.list $ cypher "StringListNullPredicateRightHandSide"]

stringListNullPredicateRightHandSide :: Binding
stringListNullPredicateRightHandSide = define "StringListNullPredicateRightHandSide" $
  T.union [
    "string">: cypher "StringPredicateExpression",
    "list">: cypher "ListPredicateExpression",
    "null">: cypher "NullPredicateExpression"]

-- StringPredicateExpression = ((SP, (S,T,A,R,T,S), SP, (W,I,T,H)) | (SP, (E,N,D,S), SP, (W,I,T,H)) | (SP, (C,O,N,T,A,I,N,S))), [SP], AddOrSubtractExpression ;

stringPredicateExpression :: Binding
stringPredicateExpression = define "StringPredicateExpression" $
  T.record [
    "operator">: cypher "StringPredicateOperator",
    "expression">: cypher "AddOrSubtractExpression"]

stringPredicateOperator :: Binding
stringPredicateOperator = define "StringPredicateOperator" $
  T.enum [
    "startsWith",
    "endsWith",
    "contains"]

-- ListPredicateExpression = SP, (I,N), [SP], AddOrSubtractExpression ;

listPredicateExpression :: Binding
listPredicateExpression = define "ListPredicateExpression" $
  T.wrap $ cypher "AddOrSubtractExpression"

-- NullPredicateExpression = (SP, (I,S), SP, (N,U,L,L))
--                         | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L))
--                         ;

nullPredicateExpression :: Binding
nullPredicateExpression = define "NullPredicateExpression" $
  T.wrap T.boolean -- true: NULL, false: NOT NULL

-- AddOrSubtractExpression = MultiplyDivideModuloExpression, { ([SP], '+', [SP], MultiplyDivideModuloExpression) | ([SP], '-', [SP], MultiplyDivideModuloExpression) } ;

addOrSubtractExpression :: Binding
addOrSubtractExpression = define "AddOrSubtractExpression" $
  T.record [
    "left">: cypher "MultiplyDivideModuloExpression",
    "right">: T.list $ cypher "AddOrSubtractRightHandSide"]

addOrSubtractRightHandSide :: Binding
addOrSubtractRightHandSide = define "AddOrSubtractRightHandSide" $
  T.record [
    "operator">: cypher "AddOrSubtractOperator",
    "expression">: cypher "MultiplyDivideModuloExpression"]

addOrSubtractOperator :: Binding
addOrSubtractOperator = define "AddOrSubtractOperator" $
  T.enum [
    "add",
    "subtract"]

-- MultiplyDivideModuloExpression = PowerOfExpression, { ([SP], '*', [SP], PowerOfExpression) | ([SP], '/', [SP], PowerOfExpression) | ([SP], '%', [SP], PowerOfExpression) } ;

multiplyDivideModuloExpression :: Binding
multiplyDivideModuloExpression = define "MultiplyDivideModuloExpression" $
  T.record [
    "left">: cypher "PowerOfExpression",
    "right">: T.list $ cypher "MultiplyDivideModuloRightHandSide"]

multiplyDivideModuloRightHandSide :: Binding
multiplyDivideModuloRightHandSide = define "MultiplyDivideModuloRightHandSide" $
  T.record [
    "operator">: cypher "MultiplyDivideModuloOperator",
    "expression">: cypher "PowerOfExpression"]

multiplyDivideModuloOperator :: Binding
multiplyDivideModuloOperator = define "MultiplyDivideModuloOperator" $
  T.enum [
    "multiply",
    "divide",
    "modulo"]

-- PowerOfExpression = UnaryAddOrSubtractExpression, { [SP], '^', [SP], UnaryAddOrSubtractExpression } ;

powerOfExpression :: Binding
powerOfExpression = define "PowerOfExpression" $
  T.wrap $ T.list $ cypher "UnaryAddOrSubtractExpression"

-- UnaryAddOrSubtractExpression = NonArithmeticOperatorExpression
--                              | (('+' | '-'), [SP], NonArithmeticOperatorExpression)
--                              ;

unaryAddOrSubtractExpression :: Binding
unaryAddOrSubtractExpression = define "UnaryAddOrSubtractExpression" $
  T.record [
    "operator">: T.maybe $ cypher "AddOrSubtractOperator",
    "expression">: cypher "NonArithmeticOperatorExpression"]

-- NonArithmeticOperatorExpression = Atom, { ([SP], ListOperatorExpression) | ([SP], PropertyLookup) }, [[SP], NodeLabels] ;

listOperatorExpressionOrPropertyLookup :: Binding
listOperatorExpressionOrPropertyLookup = define "ListOperatorExpressionOrPropertyLookup" $
  T.union [
    "list">: cypher "ListOperatorExpression",
    "property">: cypher "PropertyLookup"]

nonArithmeticOperatorExpression :: Binding
nonArithmeticOperatorExpression = define "NonArithmeticOperatorExpression" $
  T.record [
    "atom">: cypher "Atom",
    "listsAndLookups">: T.list $ cypher "ListOperatorExpressionOrPropertyLookup",
    "labels">: T.maybe $ cypher "NodeLabels"]

-- ListOperatorExpression = ('[', Expression, ']')
--                        | ('[', [Expression], '..', [Expression], ']')
--                        ;

rangeExpression :: Binding
rangeExpression = define "RangeExpression" $
   T.record [
      "start">: T.maybe $ cypher "Expression",
      "end">: T.maybe $ cypher "Expression"]

listOperatorExpression :: Binding
listOperatorExpression = define "ListOperatorExpression" $
  T.union [
    "single">: cypher "Expression",
    "range">: cypher "RangeExpression"]

-- PropertyLookup = '.', [SP], (PropertyKeyName) ;

propertyLookup :: Binding
propertyLookup = define "PropertyLookup" $
  T.wrap $ cypher "PropertyKeyName"

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

atom :: Binding
atom = define "Atom" $
  T.union [
    "literal">: cypher "Literal",
    "parameter">: cypher "Parameter",
    "case">: cypher "CaseExpression",
    "countStar">: T.unit,
    "listComprehension">: cypher "ListComprehension",
    "patternComprehension">: cypher "PatternComprehension",
    "quantifier">: cypher "Quantifier",
    "patternPredicate">: cypher "PatternPredicate",
    "parenthesized">: cypher "ParenthesizedExpression",
    "functionInvocation">: cypher "FunctionInvocation",
    "existentialSubquery">: cypher "ExistentialSubquery",
    "variable">: cypher "Variable"]

-- CaseExpression = (((C,A,S,E), { [SP], CaseAlternative }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternative }-)), [[SP], (E,L,S,E), [SP], Expression], [SP], (E,N,D) ;

caseExpression :: Binding
caseExpression = define "CaseExpression" $
  T.record [
    "expression">: T.maybe $ cypher "Expression",
    "alternatives">: T.list $ cypher "CaseAlternative",
    "else">: T.maybe $ cypher "Expression"]

-- CaseAlternative = (W,H,E,N), [SP], Expression, [SP], (T,H,E,N), [SP], Expression ;

caseAlternative :: Binding
caseAlternative = define "CaseAlternative" $
  T.record [
    "condition">: cypher "Expression",
    "result">: cypher "Expression"]

-- ListComprehension = '[', [SP], FilterExpression, [[SP], '|', [SP], Expression], [SP], ']' ;

listComprehension :: Binding
listComprehension = define "ListComprehension" $
  T.record [
    "left">: cypher "FilterExpression",
    "right">: T.maybe $ cypher "Expression"]

-- PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [Where, [SP]], '|', [SP], Expression, [SP], ']' ;

patternComprehension :: Binding
patternComprehension = define "PatternComprehension" $
  T.record [
    "variable">: T.maybe $ cypher "Variable",
    "pattern">: cypher "RelationshipsPattern",
    "where">: T.maybe $ cypher "Where",
    "right">: cypher "Expression"]

-- Quantifier = ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
--            | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
--            | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
--            | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
--            ;

quantifier :: Binding
quantifier = define "Quantifier" $
  T.record [
    "operator">: cypher "QuantifierOperator",
    "expression">: cypher "FilterExpression"]

quantifierOperator :: Binding
quantifierOperator = define "QuantifierOperator" $
  T.enum [
    "all",
    "any",
    "none",
    "single"]

-- FilterExpression = IdInColl, [[SP], Where] ;

filterExpression :: Binding
filterExpression = define "FilterExpression" $
  T.record [
    "idInColl">: cypher "IdInColl",
    "where">: T.maybe $ cypher "Where"]

-- PatternPredicate = RelationshipsPattern ;

patternPredicate :: Binding
patternPredicate = define "PatternPredicate" $
  T.wrap $ cypher "RelationshipsPattern"

-- ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;

parenthesizedExpression :: Binding
parenthesizedExpression = define "ParenthesizedExpression" $
  T.wrap $ cypher "Expression"

-- IdInColl = Variable, SP, (I,N), SP, Expression ;

idInColl :: Binding
idInColl = define "IdInColl" $
  T.record [
    "variable">: cypher "Variable",
    "expression">: cypher "Expression"]

-- FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

functionInvocation :: Binding
functionInvocation = define "FunctionInvocation" $
  T.record [
    "name">: cypher "QualifiedName",
    "distinct">: T.boolean,
    "arguments">: T.list $ cypher "Expression"]

-- FunctionName = Namespace, SymbolicName ;

qualifiedName :: Binding
qualifiedName = define "QualifiedName" $
  T.record [
    "namespace">: T.string,
    "local">: T.string]

-- ExistentialSubquery = (E,X,I,S,T,S), [SP], '{', [SP], (RegularQuery | (Pattern, [[SP], Where])), [SP], '}' ;

patternWhere :: Binding
patternWhere = define "PatternWhere" $
  T.record [
    "pattern">: cypher "Pattern",
    "where">: T.maybe $ cypher "Where"]

existentialSubquery :: Binding
existentialSubquery = define "ExistentialSubquery" $
  T.union [
    "regular">: cypher "RegularQuery",
    "pattern">: cypher "PatternWhere"]

-- ExplicitProcedureInvocation = ProcedureName, [SP], '(', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;

explicitProcedureInvocation :: Binding
explicitProcedureInvocation = define "ExplicitProcedureInvocation" $
  T.record [
    "name">: cypher "QualifiedName",
    "arguments">: T.list $ cypher "Expression"]

-- ImplicitProcedureInvocation = ProcedureName ;

implicitProcedureInvocation :: Binding
implicitProcedureInvocation = define "ImplicitProcedureInvocation" $
  T.wrap $ cypher "QualifiedName"

-- ProcedureResultField = SymbolicName ;

procedureResultField :: Binding
procedureResultField = define "ProcedureResultField" $
  T.wrap T.string

-- ProcedureName = Namespace, SymbolicName ;
--
-- Namespace = { SymbolicName, '.' } ;
--
-- Variable = SymbolicName ;

variable :: Binding
variable = define "Variable" $
  T.wrap T.string

-- Literal = BooleanLiteral
--         | (N,U,L,L)
--         | NumberLiteral
--         | StringLiteral
--         | ListLiteral
--         | MapLiteral
--         ;

literal :: Binding
literal = define "Literal" $
  T.union [
    "boolean">: T.boolean,
    "null">: T.unit,
    "number">: cypher "NumberLiteral",
    "string">: cypher "StringLiteral",
    "list">: cypher "ListLiteral",
    "map">: cypher "MapLiteral"]

-- BooleanLiteral = (T,R,U,E)
--                | (F,A,L,S,E)
--                ;
--
-- NumberLiteral = DoubleLiteral
--               | IntegerLiteral
--               ;

numberLiteral :: Binding
numberLiteral = define "NumberLiteral" $
  T.union [
    "double">: T.float64,
    "integer">: T.bigint]

-- IntegerLiteral = HexInteger
--                | OctalInteger
--                | DecimalInteger
--                ;
--
-- ... (many more lexical productions omitted)
--
-- StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
--               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'")
--               ;

stringLiteral :: Binding
stringLiteral = define "StringLiteral" $
  T.wrap T.string

-- ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;

listLiteral :: Binding
listLiteral = define "ListLiteral" $
  T.wrap $ T.list $ cypher "Expression"

-- MapLiteral = '{', [SP], [PropertyKeyName, [SP], ':', [SP], Expression, [SP], { ',', [SP], PropertyKeyName, [SP], ':', [SP], Expression, [SP] }], '}' ;

mapLiteral :: Binding
mapLiteral = define "MapLiteral" $
  T.wrap $ T.list $ cypher "KeyValuePair"

keyValuePair :: Binding
keyValuePair = define "KeyValuePair" $
  T.record [
    "key">: cypher "PropertyKeyName",
    "value">: cypher "Expression"]

-- PropertyKeyName = SchemaName ;

propertyKeyName :: Binding
propertyKeyName = define "PropertyKeyName" $
  T.wrap T.string

-- Parameter = '$', (SymbolicName | DecimalInteger) ;

parameter :: Binding
parameter = define "Parameter" $
  T.union [
    "symbolic">: T.string,
    "integer">: T.bigint]
