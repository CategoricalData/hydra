module Hydra.Sources.Tinkerpop.Gremlin where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.tinkerpop.gremlin"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A Gremlin model, based on the Gremlin ANTLR grammar "
      ++ "(master branch, as of 2024-06-30)."))}
  where
    definitions = [
      queryList,
      query,
      traversalSourceQuery,
      rootTraversalQuery,
      traversalSource,
      transactionPart,
      rootTraversal,
      traversalSourceSelfMethod,
      genericLiteralArgumentAndOptionalTraversalBiFunctionArgument,
      stringArgumentAndGenericLiteralArgument,
      stringArgumentAndOptionalGenericLiteralArgument,
      traversalSourceSpawnMethod,
      genericLiteralMapNullableArgumentOrNestedTraversal,
      serviceCall,
      serviceArguments,
      chainedTraversal,
      chainedTraversalElement,
      nestedTraversal,
      terminatedTraversal,
      traversalMethod,
      stringArgumentOrNestedTraversal,
      optionalTraversalScopeArgumentAndStringArgument,
      stringArgumentAndOptionalStringLiteralVarargs,
      traversalSackMethodArgumentOrIntegerArgument,
      byArgs,
      byOtherArgs,
      traversalFunctionArgumentOrStringArgumentOrNestedTraversal,
      chooseArgs,
      predicateTraversalArgument,
      nestedTraversalArgument,
      dedupArgs,
      scopeStringArgument,
      predicateOrTraversal,
      genericLiteralArgumentAndTraversalBiFunctionArgument,
      fromArgs,
      hasArgs,
      hasStringArgumentAndOptionalStringLiteralVarargs,
      hasStringArgumentAndOptionalStringLiteralVarargsRest,
      stringNullableArgumentAndGenericLiteralArgument,
      stringNullableArgumentAndTraversalPredicate,
      hasTraversalTokenArgs,
      hasTraversalTokenArgsRest,
      genericLiteralArgumentAndTraversalPredicate,
      traversalPredicateOrStringLiteralVarargs,
      traversalPredicateOrGenericLiteralArgument,
      optionArgs,
      traversalPredicateAndNestedTraversal,
      traversalMergeArgumentAndGenericLiteralMapNullableArgument,
      traversalMergeArgumentAndNestedTraversal,
      genericLiteralArgumentAndNestedTraversal,
      propertyArgs,
      traversalCardinalityArgumentAndObjects,
      genericLiteralMapNullableArgumentAndTraversalCardinalityArgument,
      rangeArgs,
      optionalStringArgumentAndNestedTraversal,
      optionalTraversalScopeArgumentAndIntegerArgument,
      selectArgs,
      popStringsArgument,
      traversalPopArgumentAndNestedTraversal,
      tailArgs,
      toArgs,
      directionAndVarargs,
      valueMapArgs,
      valueMapBooleanArgs,
      whereArgs,
      whereWithPredicateArgs,
      withArgs,
      withArgsKeys,
      withArgsValues,
      concatArgs,
      replaceArgs,
      splitArgs,
      substringArgs,
      dateAddArgs,
      dateDiffArgs,
      structureVertex,
      traversalStrategy,
      configuration,
      keywordOrIdentifier,
      traversalScope,
      traversalToken,
      traversalMerge,
      traversalOrder,
      traversalDirection,
      traversalCardinality,
      traversalColumn,
      traversalPop,
      traversalOperator,
      traversalPick,
      traversalDT,
      traversalPredicate,
      twoTraversalPredicates,
      traversalTerminalMethod,
      traversalSelfMethod,
      traversalFunction,
      rangeArgument,
      withOptionKeys,
      connectedComponentConstants,
      pageRankConstants,
      peerPressureConstants,
      shortestPathConstants,
      withOptionsValues,
      ioOptionsKeys,
      ioOptionsValues,
      booleanArgument,
      integerArgument,
      floatArgument,
      stringArgument,
      stringNullableArgument,
      dateArgument,
      genericLiteralArgument,
      genericLiteralListArgument,
      genericLiteralMapArgument,
      genericLiteralMapNullableArgument,
      structureVertexArgument,
      traversalCardinalityArgument,
      traversalColumnArgument,
      traversalDirectionArgument,
      traversalMergeArgument,
      traversalOrderArgument,
      traversalPopArgument,
      traversalSackMethodArgument,
      traversalScopeArgument,
      traversalTokenArgument,
      traversalComparatorArgument,
      traversalFunctionArgument,
      traversalBiFunctionArgument,
      traversalDTArgument,
      genericLiteralList,
      genericLiteralRange,
      integerRange,
      stringRange,
      genericLiteralSet,
      genericLiteralCollection,
      genericLiteral,
      genericLiteralMap,
      mapEntry,
      mapKey,
      integerLiteral,
      floatLiteral,
      numericLiteral,
      dateLiteral,
      keyword,
      identifier]


-- Helper for argument types (value or variable)
defArgument :: String -> Type -> TypeDefinition
defArgument name typ = define name $ T.union [
  "value">: typ,
  "variable">: gremlin "Identifier"]


gremlin :: String -> Type
gremlin = typeref ns

-- Type definitions

byArgs :: TypeDefinition
byArgs = define "ByArgs" $ T.union [
  "order">: gremlin "TraversalOrderArgument",
  "token">: gremlin "TraversalTokenArgument",
  "other">: gremlin "ByOtherArgs"]

byOtherArgs :: TypeDefinition
byOtherArgs = define "ByOtherArgs" $ T.union [
  "comparator">: T.optional $ gremlin "TraversalComparatorArgument",
  "other">: T.optional $ gremlin "TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"]

chainedTraversal :: TypeDefinition
chainedTraversal = define "ChainedTraversal" $ T.record [
  "first">: gremlin "TraversalMethod",
  "rest">: gremlin "ChainedTraversalElement"]

chainedTraversalElement :: TypeDefinition
chainedTraversalElement = define "ChainedTraversalElement" $ T.union [
  "method">: gremlin "TraversalMethod",
  "self">: gremlin "TraversalSelfMethod"]

chooseArgs :: TypeDefinition
chooseArgs = define "ChooseArgs" $ T.union [
  "function">: gremlin "TraversalFunctionArgument",
  "predicateTraversal">: gremlin "PredicateTraversalArgument",
  "traversal">: gremlin "NestedTraversalArgument"]

concatArgs :: TypeDefinition
concatArgs = define "ConcatArgs" $ T.union [
  "traversal">: nonemptyList $ gremlin "NestedTraversal",
  "string">: nonemptyList $ gremlin "StringNullableArgument"]

configuration :: TypeDefinition
configuration = define "Configuration" $ T.record [
  "key">: gremlin "KeywordOrIdentifier",
  "value">: gremlin "GenericLiteralArgument"]

connectedComponentConstants :: TypeDefinition
connectedComponentConstants = define "ConnectedComponentConstants" $ T.enum [
  "component",
  "edges",
  "propertyName"]

dateAddArgs :: TypeDefinition
dateAddArgs = define "DateAddArgs" $ T.record [
  "unit">: gremlin "TraversalDTArgument",
  "duration">: gremlin "IntegerArgument"]

dateDiffArgs :: TypeDefinition
dateDiffArgs = define "DateDiffArgs" $ T.union [
  "traversal">: gremlin "NestedTraversal",
  "date">: gremlin "DateArgument"]

dedupArgs :: TypeDefinition
dedupArgs = define "DedupArgs" $ T.union [
  "scopeString">: gremlin "ScopeStringArgument",
  "string">: nonemptyList $ gremlin "StringNullableArgument"]

directionAndVarargs :: TypeDefinition
directionAndVarargs = define "DirectionAndVarargs" $ T.record [
  "direction">: gremlin "TraversalDirectionArgument",
  "varargs">: T.list $ gremlin "StringNullableArgument"]

fromArgs :: TypeDefinition
fromArgs = define "FromArgs" $ T.union [
  "string">: gremlin "StringArgument",
  "vertex">: gremlin "StructureVertexArgument",
  "traversal">: gremlin "NestedTraversal"]

genericLiteralArgumentAndNestedTraversal :: TypeDefinition
genericLiteralArgumentAndNestedTraversal = define "GenericLiteralArgumentAndNestedTraversal" $ T.record [
  "object">: gremlin "GenericLiteralArgument",
  "traversal">: gremlin "NestedTraversal"]

genericLiteralArgumentAndOptionalTraversalBiFunctionArgument :: TypeDefinition
genericLiteralArgumentAndOptionalTraversalBiFunctionArgument = define "GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument" $ T.record [
  "literal">: gremlin "GenericLiteralArgument",
  "biFunction">: T.optional $ gremlin "TraversalBiFunctionArgument"]

genericLiteralArgumentAndTraversalBiFunctionArgument :: TypeDefinition
genericLiteralArgumentAndTraversalBiFunctionArgument = define "GenericLiteralArgumentAndTraversalBiFunctionArgument" $ T.record [
  "literal">: gremlin "GenericLiteralArgument",
  "biFunction">: gremlin "TraversalBiFunctionArgument"]

genericLiteralArgumentAndTraversalPredicate :: TypeDefinition
genericLiteralArgumentAndTraversalPredicate = define "GenericLiteralArgumentAndTraversalPredicate" $ T.union [
  "literal">: gremlin "GenericLiteralArgument",
  "predicate">: gremlin "TraversalPredicate"]

genericLiteralMapNullableArgumentAndTraversalCardinalityArgument :: TypeDefinition
genericLiteralMapNullableArgumentAndTraversalCardinalityArgument = define "GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument" $ T.record [
  "cardinality">: gremlin "TraversalCardinalityArgument",
  "object">: gremlin "GenericLiteralMapNullableArgument"]

genericLiteralMapNullableArgumentOrNestedTraversal :: TypeDefinition
genericLiteralMapNullableArgumentOrNestedTraversal = define "GenericLiteralMapNullableArgumentOrNestedTraversal" $ T.union [
  "map">: gremlin "GenericLiteralMapNullableArgument",
  "traversal">: gremlin "NestedTraversal"]

hasArgs :: TypeDefinition
hasArgs = define "HasArgs" $ T.union [
  "string">: gremlin "HasStringArgumentAndOptionalStringLiteralVarargs",
  "traversalToken">: gremlin "HasTraversalTokenArgs"]

hasStringArgumentAndOptionalStringLiteralVarargs :: TypeDefinition
hasStringArgumentAndOptionalStringLiteralVarargs = define "HasStringArgumentAndOptionalStringLiteralVarargs" $ T.record [
  "string">: gremlin "StringNullableArgument",
  "rest">: T.optional $ gremlin "HasStringArgumentAndOptionalStringLiteralVarargsRest"]

hasStringArgumentAndOptionalStringLiteralVarargsRest :: TypeDefinition
hasStringArgumentAndOptionalStringLiteralVarargsRest = define "HasStringArgumentAndOptionalStringLiteralVarargsRest" $ T.union [
  "object">: gremlin "GenericLiteralArgument",
  "predicate">: gremlin "TraversalPredicate",
  "stringObject">: gremlin "StringNullableArgumentAndGenericLiteralArgument",
  "stringPredicate">: gremlin "StringNullableArgumentAndTraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

hasTraversalTokenArgs :: TypeDefinition
hasTraversalTokenArgs = define "HasTraversalTokenArgs" $ T.record [
  "traversalToken">: gremlin "TraversalTokenArgument",
  "rest">: gremlin "HasTraversalTokenArgsRest"]

hasTraversalTokenArgsRest :: TypeDefinition
hasTraversalTokenArgsRest = define "HasTraversalTokenArgsRest" $ T.union [
  "literal">: gremlin "GenericLiteralArgument",
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

ioOptionsKeys :: TypeDefinition
ioOptionsKeys = define "IoOptionsKeys" $ T.enum [
  "reader",
  "writer"]

ioOptionsValues :: TypeDefinition
ioOptionsValues = define "IoOptionsValues" $ T.enum [
  "gryo",
  "graphson",
  "graphml"]

keywordOrIdentifier :: TypeDefinition
keywordOrIdentifier = define "KeywordOrIdentifier" $ T.union [
  "keyword">: gremlin "Keyword",
  "identifier">: gremlin "Identifier"]

nestedTraversal :: TypeDefinition
nestedTraversal = define "NestedTraversal" $ T.union [
  "root">: gremlin "RootTraversal",
  "chained">: gremlin "ChainedTraversal",
  "anonymous">: gremlin "ChainedTraversal"]

nestedTraversalArgument :: TypeDefinition
nestedTraversalArgument = define "NestedTraversalArgument" $ T.record [
  "traversal1">: gremlin "NestedTraversal",
  "traversal2">: T.optional $ gremlin "NestedTraversal",
  "traversal3">: T.optional $ gremlin "NestedTraversal"]

optionArgs :: TypeDefinition
optionArgs = define "OptionArgs" $ T.union [
  "predicateTraversal">: gremlin "TraversalPredicateAndNestedTraversal",
  "mergeMap">: gremlin "TraversalMergeArgumentAndGenericLiteralMapNullableArgument",
  "mergeTraversal">: gremlin "TraversalMergeArgumentAndNestedTraversal",
  "objectTraversal">: gremlin "GenericLiteralArgumentAndNestedTraversal",
  "traversal">: gremlin "NestedTraversal"]

optionalStringArgumentAndNestedTraversal :: TypeDefinition
optionalStringArgumentAndNestedTraversal = define "OptionalStringArgumentAndNestedTraversal" $ T.record [
  "string">: T.optional $ gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

optionalTraversalScopeArgumentAndIntegerArgument :: TypeDefinition
optionalTraversalScopeArgumentAndIntegerArgument = define "OptionalTraversalScopeArgumentAndIntegerArgument" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "long">: gremlin "IntegerArgument"]

optionalTraversalScopeArgumentAndStringArgument :: TypeDefinition
optionalTraversalScopeArgumentAndStringArgument = define "OptionalTraversalScopeArgumentAndStringArgument" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "string">: gremlin "StringArgument"]

pageRankConstants :: TypeDefinition
pageRankConstants = define "PageRankConstants" $ T.enum [
  "edges",
  "times",
  "propertyName"]

peerPressureConstants :: TypeDefinition
peerPressureConstants = define "PeerPressureConstants" $ T.enum [
  "edges",
  "times",
  "propertyName"]

popStringsArgument :: TypeDefinition
popStringsArgument = define "PopStringsArgument" $ T.record [
  "pop">: gremlin "TraversalPopArgument",
  "string">: nonemptyList $ gremlin "StringArgument"]

predicateOrTraversal :: TypeDefinition
predicateOrTraversal = define "PredicateOrTraversal" $ T.union [
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

predicateTraversalArgument :: TypeDefinition
predicateTraversalArgument = define "PredicateTraversalArgument" $ T.record [
  "predicate">: gremlin "TraversalPredicate",
  "traversal1">: gremlin "NestedTraversal",
  "traversal2">: T.optional $ gremlin "NestedTraversal"]

propertyArgs :: TypeDefinition
propertyArgs = define "PropertyArgs" $ T.union [
  "cardinalityObjects">: gremlin "TraversalCardinalityArgumentAndObjects",
  "objects">: minLengthList 2 $ gremlin "GenericLiteralArgument",
  "object">: gremlin "GenericLiteralMapNullableArgument",
  "cardinalityObject">: gremlin "GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"]

query :: TypeDefinition
query = define "Query" $ T.union [
  "traversalSource">: gremlin "TraversalSourceQuery",
  "rootTraversal">: gremlin "RootTraversalQuery",
  "toString">: T.unit,
  "empty">: T.unit]

queryList :: TypeDefinition
queryList = define "QueryList" $ T.wrap $ nonemptyList $ gremlin "Query"

rangeArgs :: TypeDefinition
rangeArgs = define "RangeArgs" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "min">: gremlin "IntegerArgument",
  "max">: gremlin "IntegerArgument"]

rangeArgument :: TypeDefinition
rangeArgument = define "RangeArgument" $ T.record [
  "min">: gremlin "GenericLiteralArgument",
  "max">: gremlin "GenericLiteralArgument"]

replaceArgs :: TypeDefinition
replaceArgs = define "ReplaceArgs" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "from">: gremlin "StringNullableArgument",
  "to">: gremlin "StringNullableArgument"]

rootTraversal :: TypeDefinition
rootTraversal = define "RootTraversal" $ T.record [
  "source">: gremlin "TraversalSource",
  "spawnMethod">: gremlin "TraversalSourceSpawnMethod",
  "chained">: T.list $ gremlin "ChainedTraversalElement"]

rootTraversalQuery :: TypeDefinition
rootTraversalQuery = define "RootTraversalQuery" $ T.record [
  "root">: gremlin "RootTraversal",
  "terminalMethod">: T.optional $ gremlin "TraversalTerminalMethod"]

scopeStringArgument :: TypeDefinition
scopeStringArgument = define "ScopeStringArgument" $ T.record [
  "scope">: gremlin "TraversalScopeArgument",
  "strings">: T.list $ gremlin "StringNullableArgument"]

selectArgs :: TypeDefinition
selectArgs = define "SelectArgs" $ T.union [
  "column">: gremlin "TraversalColumnArgument",
  "popStrings">: gremlin "PopStringsArgument",
  "popTraversal">: gremlin "TraversalPopArgumentAndNestedTraversal",
  "strings">: nonemptyList $ gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

serviceArguments :: TypeDefinition
serviceArguments = define "ServiceArguments" $ T.union [
  "map">: T.optional $ gremlin "GenericLiteralMapArgument",
  "traversal">: T.optional $ gremlin "NestedTraversal"]

serviceCall :: TypeDefinition
serviceCall = define "ServiceCall" $ T.record [
  "service">: gremlin "StringArgument",
  "arguments">: gremlin "ServiceArguments"]

shortestPathConstants :: TypeDefinition
shortestPathConstants = define "ShortestPathConstants" $ T.enum [
  "target",
  "edges",
  "distance",
  "maxDistance",
  "includeEdges"]

splitArgs :: TypeDefinition
splitArgs = define "SplitArgs" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "delimiter">: gremlin "StringNullableArgument"]

stringArgumentAndGenericLiteralArgument :: TypeDefinition
stringArgumentAndGenericLiteralArgument = define "StringArgumentAndGenericLiteralArgument" $ T.record [
  "string">: gremlin "StringArgument",
  "literal">: gremlin "GenericLiteralArgument"]

stringArgumentAndOptionalGenericLiteralArgument :: TypeDefinition
stringArgumentAndOptionalGenericLiteralArgument = define "StringArgumentAndOptionalGenericLiteralArgument" $ T.record [
  "string">: gremlin "StringArgument",
  "literal">: T.optional $ gremlin "GenericLiteralArgument"]

stringArgumentAndOptionalStringLiteralVarargs :: TypeDefinition
stringArgumentAndOptionalStringLiteralVarargs = define "StringArgumentAndOptionalStringLiteralVarargs" $ T.record [
  "first">: gremlin "StringArgument",
  "rest">: T.list $ gremlin "StringNullableArgument"]

stringArgumentOrNestedTraversal :: TypeDefinition
stringArgumentOrNestedTraversal = define "StringArgumentOrNestedTraversal" $ T.union [
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

stringNullableArgumentAndGenericLiteralArgument :: TypeDefinition
stringNullableArgumentAndGenericLiteralArgument = define "StringNullableArgumentAndGenericLiteralArgument" $ T.record [
  "string">: gremlin "StringNullableArgument",
  "literal">: gremlin "GenericLiteralArgument"]

stringNullableArgumentAndTraversalPredicate :: TypeDefinition
stringNullableArgumentAndTraversalPredicate = define "StringNullableArgumentAndTraversalPredicate" $ T.record [
  "string">: gremlin "StringNullableArgument",
  "predicate">: gremlin "TraversalPredicate"]

structureVertex :: TypeDefinition
structureVertex = define "StructureVertex" $ T.record [
  "new">: T.boolean,
  "id">: gremlin "GenericLiteralArgument",
  "label">: gremlin "StringArgument"]

substringArgs :: TypeDefinition
substringArgs = define "SubstringArgs" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "start">: gremlin "IntegerArgument",
  "end">: T.optional $ gremlin "IntegerArgument"]

tailArgs :: TypeDefinition
tailArgs = define "TailArgs" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "integer">: T.optional $ gremlin "IntegerArgument"]

terminatedTraversal :: TypeDefinition
terminatedTraversal = define "TerminatedTraversal" $ T.record [
  "root">: gremlin "RootTraversal",
  "terminal">: gremlin "TraversalTerminalMethod"]

toArgs :: TypeDefinition
toArgs = define "ToArgs" $ T.union [
  "direction">: gremlin "DirectionAndVarargs",
  "string">: gremlin "StringArgument",
  "vertex">: gremlin "StructureVertexArgument",
  "traversal">: gremlin "NestedTraversal"]

transactionPart :: TypeDefinition
transactionPart = define "TransactionPart" $ T.enum [
  "begin",
  "commit",
  "rollback"]

traversalCardinality :: TypeDefinition
traversalCardinality = define "TraversalCardinality" $ T.union [
  "single">: gremlin "GenericLiteral",
  "set">: gremlin "GenericLiteral",
  "list">: gremlin "GenericLiteral"]

traversalCardinalityArgumentAndObjects :: TypeDefinition
traversalCardinalityArgumentAndObjects = define "TraversalCardinalityArgumentAndObjects" $ T.record [
  "cardinality">: gremlin "TraversalCardinalityArgument",
  "objects">: minLengthList 2 $ gremlin "GenericLiteralArgument"]

traversalColumn :: TypeDefinition
traversalColumn = define "TraversalColumn" $ T.enum [
  "keys",
  "values"]

traversalDT :: TypeDefinition
traversalDT = define "TraversalDT" $ T.enum [
  "second",
  "minute",
  "hour",
  "day"]

traversalDirection :: TypeDefinition
traversalDirection = define "TraversalDirection" $ T.enum [
  "in",
  "out",
  "both"]

traversalFunction :: TypeDefinition
traversalFunction = define "TraversalFunction" $ T.union [
  "token">: gremlin "TraversalToken",
  "column">: gremlin "TraversalColumn"]

traversalFunctionArgumentOrStringArgumentOrNestedTraversal :: TypeDefinition
traversalFunctionArgumentOrStringArgumentOrNestedTraversal = define "TraversalFunctionArgumentOrStringArgumentOrNestedTraversal" $ T.union [
  "function">: gremlin "TraversalFunctionArgument",
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

traversalMerge :: TypeDefinition
traversalMerge = define "TraversalMerge" $ T.enum [
  "onCreate",
  "onMatch",
  "outV",
  "inV"]

traversalMergeArgumentAndGenericLiteralMapNullableArgument :: TypeDefinition
traversalMergeArgumentAndGenericLiteralMapNullableArgument = define "TraversalMergeArgumentAndGenericLiteralMapNullableArgument" $ T.record [
  "merge">: gremlin "TraversalMergeArgument",
  "map">: gremlin "GenericLiteralMapNullableArgument",
  "cardinality">: T.optional $ gremlin "TraversalCardinality"]

traversalMergeArgumentAndNestedTraversal :: TypeDefinition
traversalMergeArgumentAndNestedTraversal = define "TraversalMergeArgumentAndNestedTraversal" $ T.record [
  "merge">: gremlin "TraversalMergeArgument",
  "traversal">: gremlin "NestedTraversal"]

traversalMethod :: TypeDefinition
traversalMethod = define "TraversalMethod" $ T.union [
  "v">: nonemptyList $ gremlin "GenericLiteralArgument",
  "e">: nonemptyList $ gremlin "GenericLiteralArgument",
  "addE">: gremlin "StringArgumentOrNestedTraversal",
  "addV">: T.optional $ gremlin "StringArgumentOrNestedTraversal",
  "mergeE">: T.optional $ gremlin "GenericLiteralMapNullableArgumentOrNestedTraversal",
  "mergeV">: T.optional $ gremlin "GenericLiteralMapNullableArgumentOrNestedTraversal",
  "aggregate">: gremlin "OptionalTraversalScopeArgumentAndStringArgument",
  "all">: gremlin "TraversalPredicate",
  "and">: T.list $ gremlin "NestedTraversal",
  "any">: gremlin "TraversalPredicate",
  "as">: gremlin "StringArgumentAndOptionalStringLiteralVarargs",
  "barrier">: T.optional $ gremlin "TraversalSackMethodArgumentOrIntegerArgument",
  "both">: nonemptyList $ gremlin "StringNullableArgument",
  "bothE">: nonemptyList $ gremlin "StringNullableArgument",
  "bothV">: T.unit,
  "branch">: gremlin "NestedTraversal",
  "by">: gremlin "ByArgs",
  "cap">: gremlin "StringArgumentAndOptionalStringLiteralVarargs",
  "choose">: gremlin "ChooseArgs",
  "coalesce">: T.list $ gremlin "NestedTraversal",
  "coin">: gremlin "FloatArgument",
  "conjoin">: gremlin "StringArgument",
  "connectedComponent">: T.unit,
  "constant">: gremlin "GenericLiteralArgument",
  "count">: T.optional $ gremlin "TraversalScopeArgument",
  "cyclicPath">: T.unit,
  "dedup">: gremlin "DedupArgs",
  "difference">: gremlin "GenericLiteralArgument",
  "disjunct">: gremlin "GenericLiteralArgument",
  "drop">: T.unit,
  "elementMap">: nonemptyList $ gremlin "StringNullableArgument",
  "emit">: T.optional $ gremlin "PredicateOrTraversal",
  "filter">: gremlin "PredicateOrTraversal",
  "flatMap">: gremlin "NestedTraversal",
  "fold">: T.optional $ gremlin "GenericLiteralArgumentAndTraversalBiFunctionArgument",
  "from">: gremlin "FromArgs",
  "group">: T.optional $ gremlin "StringArgument",
  "groupCount">: T.optional $ gremlin "StringArgument",
  "has">: gremlin "HasArgs",
  "hasId">: gremlin "GenericLiteralArgumentAndTraversalPredicate",
  "hasKey">: gremlin "TraversalPredicateOrStringLiteralVarargs",
  "hasLabel">: gremlin "TraversalPredicateOrStringLiteralVarargs",
  "hasNot">: gremlin "StringNullableArgument",
  "hasValue">: gremlin "TraversalPredicateOrGenericLiteralArgument",
  "id">: T.unit,
  "identity">: T.unit,
  "in">: nonemptyList $ gremlin "StringNullableArgument",
  "inE">: nonemptyList $ gremlin "StringNullableArgument",
  "intersect">: gremlin "GenericLiteralArgument",
  "inV">: T.unit,
  "index">: T.unit,
  "inject">: nonemptyList $ gremlin "GenericLiteralArgument",
  "is">: gremlin "TraversalPredicateOrGenericLiteralArgument",
  "key">: T.unit,
  "label">: T.unit,
  "limit">: gremlin "OptionalTraversalScopeArgumentAndIntegerArgument",
  "local">: gremlin "NestedTraversal",
  "loops">: T.optional $ gremlin "StringArgument",
  "map">: gremlin "NestedTraversal",
  "match">: T.list $ gremlin "NestedTraversal",
  "math">: gremlin "StringArgument",
  "max">: T.optional $ gremlin "TraversalScopeArgument",
  "mean">: T.optional $ gremlin "TraversalScopeArgument",
  "min">: T.optional $ gremlin "TraversalScopeArgument",
  "none">: gremlin "TraversalPredicate",
  "not">: gremlin "NestedTraversal",
  "option">: gremlin "OptionArgs",
  "optional">: gremlin "NestedTraversal",
  "or">: T.list $ gremlin "NestedTraversal",
  "order">: T.optional $ gremlin "TraversalScopeArgument",
  "otherV">: T.unit,
  "out">: nonemptyList $ gremlin "StringNullableArgument",
  "outE">: nonemptyList $ gremlin "StringNullableArgument",
  "outV">: T.unit,
  "pageRank">: T.optional $ gremlin "FloatArgument",
  "path">: T.unit,
  "peerPressure">: T.unit,
  "profile">: T.optional $ gremlin "StringArgument",
  "project">: gremlin "StringArgumentAndOptionalStringLiteralVarargs",
  "properties">: nonemptyList $ gremlin "StringNullableArgument",
  "property">: gremlin "PropertyArgs",
  "propertyMap">: nonemptyList $ gremlin "StringNullableArgument",
  "range">: gremlin "RangeArgs",
  "read">: T.unit,
  "repeat">: gremlin "OptionalStringArgumentAndNestedTraversal",
  "sack">: T.optional $ gremlin "TraversalBiFunctionArgument",
  "sample">: gremlin "OptionalTraversalScopeArgumentAndIntegerArgument",
  "select">: gremlin "SelectArgs",
  "combine">: gremlin "GenericLiteralArgument",
  "product">: gremlin "GenericLiteralArgument",
  "merge">: gremlin "GenericLiteralArgument",
  "shortestPath">: T.unit,
  "sideEffect">: gremlin "NestedTraversal",
  "simplePath">: T.unit,
  "skip">: gremlin "OptionalTraversalScopeArgumentAndIntegerArgument",
  "store">: gremlin "StringArgument",
  "subgraph">: gremlin "StringArgument",
  "sum">: T.optional $ gremlin "TraversalScopeArgument",
  "tail">: T.optional $ gremlin "TailArgs",
  "fail">: T.optional $ gremlin "StringArgument",
  "times">: gremlin "IntegerArgument",
  "to">: gremlin "ToArgs",
  "toE">: gremlin "DirectionAndVarargs",
  "toV">: gremlin "TraversalDirectionArgument",
  "tree">: T.optional $ gremlin "StringArgument",
  "unfold">: T.unit,
  "union">: T.list $ gremlin "NestedTraversal",
  "until">: gremlin "PredicateOrTraversal",
  "value">: T.unit,
  "valueMap">: gremlin "ValueMapArgs",
  "values">: nonemptyList $ gremlin "StringNullableArgument",
  "where">: gremlin "WhereArgs",
  "with">: gremlin "WithArgs",
  "write">: T.unit,
  "element">: nonemptyList $ gremlin "StringNullableArgument",
  "call">: gremlin "ServiceCall",
  "concat">: gremlin "ConcatArgs",
  "asString">: T.optional $ gremlin "TraversalScopeArgument",
  "format">: gremlin "StringArgument",
  "toUpper">: T.optional $ gremlin "TraversalScopeArgument",
  "toLower">: T.optional $ gremlin "TraversalScopeArgument",
  "length">: T.optional $ gremlin "TraversalScopeArgument",
  "trim">: T.optional $ gremlin "TraversalScopeArgument",
  "lTrim">: T.optional $ gremlin "TraversalScopeArgument",
  "rTrim">: T.optional $ gremlin "TraversalScopeArgument",
  "reverse">: T.unit,
  "replace">: gremlin "ReplaceArgs",
  "split">: gremlin "SplitArgs",
  "substring">: gremlin "SubstringArgs",
  "asDate">: T.unit,
  "dateAdd">: gremlin "DateAddArgs",
  "dateDiff">: gremlin "DateDiffArgs"]

traversalOperator :: TypeDefinition
traversalOperator = define "TraversalOperator" $ T.enum [
  "addAll",
  "and",
  "assign",
  "div",
  "max",
  "min",
  "minus",
  "mult",
  "or",
  "sum",
  "sumLong"]

traversalOrder :: TypeDefinition
traversalOrder = define "TraversalOrder" $ T.enum [
  "incr",
  "decr",
  "asc",
  "desc",
  "shuffle"]

traversalPick :: TypeDefinition
traversalPick = define "TraversalPick" $ T.enum [
  "any",
  "none"]

traversalPop :: TypeDefinition
traversalPop = define "TraversalPop" $ T.enum [
  "first",
  "last",
  "all",
  "mixed"]

traversalPopArgumentAndNestedTraversal :: TypeDefinition
traversalPopArgumentAndNestedTraversal = define "TraversalPopArgumentAndNestedTraversal" $ T.record [
  "pop">: gremlin "TraversalPopArgument",
  "traversal">: gremlin "NestedTraversal"]

traversalPredicate :: TypeDefinition
traversalPredicate = define "TraversalPredicate" $ T.union [
  "eq">: gremlin "GenericLiteralArgument",
  "neq">: gremlin "GenericLiteralArgument",
  "lt">: gremlin "GenericLiteralArgument",
  "lte">: gremlin "GenericLiteralArgument",
  "gt">: gremlin "GenericLiteralArgument",
  "gte">: gremlin "GenericLiteralArgument",
  "inside">: gremlin "RangeArgument",
  "outside">: gremlin "RangeArgument",
  "between">: gremlin "RangeArgument",
  "within">: T.optional $ gremlin "GenericLiteralArgument",
  "without">: T.optional $ gremlin "GenericLiteralArgument",
  "not">: gremlin "TraversalPredicate",
  "startingWith">: gremlin "StringArgument",
  "notStartingWith">: gremlin "StringArgument",
  "endingWith">: gremlin "StringArgument",
  "notEndingWith">: gremlin "StringArgument",
  "containing">: gremlin "StringArgument",
  "notContaining">: gremlin "StringArgument",
  "regex">: gremlin "StringArgument",
  "notRegex">: gremlin "StringArgument",
  "and">: gremlin "TwoTraversalPredicates",
  "or">: gremlin "TwoTraversalPredicates",
  "negate">: gremlin "TraversalPredicate"]

traversalPredicateAndNestedTraversal :: TypeDefinition
traversalPredicateAndNestedTraversal = define "TraversalPredicateAndNestedTraversal" $ T.record [
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

traversalPredicateOrGenericLiteralArgument :: TypeDefinition
traversalPredicateOrGenericLiteralArgument = define "TraversalPredicateOrGenericLiteralArgument" $ T.union [
  "predicate">: gremlin "TraversalPredicate",
  "literal">: T.list $ gremlin "GenericLiteralArgument"]

traversalPredicateOrStringLiteralVarargs :: TypeDefinition
traversalPredicateOrStringLiteralVarargs = define "TraversalPredicateOrStringLiteralVarargs" $ T.union [
  "predicate">: gremlin "TraversalPredicate",
  "string">: nonemptyList $ gremlin "StringNullableArgument"]

traversalSackMethodArgumentOrIntegerArgument :: TypeDefinition
traversalSackMethodArgumentOrIntegerArgument = define "TraversalSackMethodArgumentOrIntegerArgument" $ T.union [
  "consumer">: gremlin "TraversalSackMethodArgument",
  "int">: gremlin "IntegerArgument"]

traversalScope :: TypeDefinition
traversalScope = define "TraversalScope" $ T.enum [
  "local",
  "global"]

traversalSelfMethod :: TypeDefinition
traversalSelfMethod = define "TraversalSelfMethod" $ T.enum [
  "discard"]

traversalSource :: TypeDefinition
traversalSource = define "TraversalSource" $ T.wrap $ T.list $ gremlin "TraversalSourceSelfMethod"

traversalSourceQuery :: TypeDefinition
traversalSourceQuery = define "TraversalSourceQuery" $ T.record [
  "source">: gremlin "TraversalSource",
  "transactionPart">: T.optional $ gremlin "TransactionPart"]

traversalSourceSelfMethod :: TypeDefinition
traversalSourceSelfMethod = define "TraversalSourceSelfMethod" $ T.union [
  "withBulk">: T.boolean,
  "withPath">: T.unit,
  "withSack">: gremlin "GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument",
  "withSideEffect">: gremlin "StringArgumentAndGenericLiteralArgument",
  "withStrategies">: nonemptyList $ gremlin "TraversalStrategy",
  "withoutStrategies">: nonemptyList $ gremlin "Identifier",
  "with">: gremlin "StringArgumentAndOptionalGenericLiteralArgument"]

traversalSourceSpawnMethod :: TypeDefinition
traversalSourceSpawnMethod = define "TraversalSourceSpawnMethod" $ T.union [
  "addE">: gremlin "StringArgumentOrNestedTraversal",
  "addV">: T.optional $ gremlin "StringArgumentOrNestedTraversal",
  "e">: nonemptyList $ gremlin "GenericLiteralArgument",
  "v">: nonemptyList $ gremlin "GenericLiteralArgument",
  "mergeV">: gremlin "GenericLiteralMapNullableArgumentOrNestedTraversal",
  "mergeE">: gremlin "GenericLiteralMapNullableArgumentOrNestedTraversal",
  "inject">: nonemptyList $ gremlin "GenericLiteralArgument",
  "io">: gremlin "StringArgument",
  "call">: T.optional $ gremlin "ServiceCall",
  "union">: T.list $ gremlin "NestedTraversal"]

traversalStrategy :: TypeDefinition
traversalStrategy = define "TraversalStrategy" $ T.record [
  "new">: T.boolean,
  "class">: gremlin "Identifier",
  "configurations">: T.list $ gremlin "Configuration"]

traversalTerminalMethod :: TypeDefinition
traversalTerminalMethod = define "TraversalTerminalMethod" $ T.union [
  "explain">: T.unit,
  "iterate">: T.unit,
  "hasNext">: T.unit,
  "tryNext">: T.unit,
  "next">: T.optional $ gremlin "IntegerLiteral",
  "toList">: T.unit,
  "toSet">: T.unit,
  "toBulkSet">: T.unit]

traversalToken :: TypeDefinition
traversalToken = define "TraversalToken" $ T.enum [
  "id",
  "label",
  "key",
  "value"]

twoTraversalPredicates :: TypeDefinition
twoTraversalPredicates = define "TwoTraversalPredicates" $ T.record [
  "left">: gremlin "TraversalPredicate",
  "right">: gremlin "TraversalPredicate"]

valueMapArgs :: TypeDefinition
valueMapArgs = define "ValueMapArgs" $ T.union [
  "string">: nonemptyList $ gremlin "StringNullableArgument",
  "boolean">: gremlin "ValueMapBooleanArgs"]

valueMapBooleanArgs :: TypeDefinition
valueMapBooleanArgs = define "ValueMapBooleanArgs" $ T.record [
  "value">: gremlin "BooleanArgument",
  "keys">: T.optional $ nonemptyList $ gremlin "StringNullableArgument"]

whereArgs :: TypeDefinition
whereArgs = define "WhereArgs" $ T.union [
  "predicate">: gremlin "WhereWithPredicateArgs",
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

whereWithPredicateArgs :: TypeDefinition
whereWithPredicateArgs = define "WhereWithPredicateArgs" $ T.record [
  "leftArg">: T.optional $ gremlin "StringArgument",
  "predicate">: gremlin "TraversalPredicate"]

withArgs :: TypeDefinition
withArgs = define "WithArgs" $ T.record [
  "keys">: gremlin "WithArgsKeys",
  "values">: T.optional $ gremlin "WithArgsValues"]

withArgsKeys :: TypeDefinition
withArgsKeys = define "WithArgsKeys" $ T.union [
  "withOption">: gremlin "WithOptionKeys",
  "string">: gremlin "StringArgument"]

withArgsValues :: TypeDefinition
withArgsValues = define "WithArgsValues" $ T.union [
  "withOptions">: gremlin "WithOptionsValues",
  "io">: gremlin "IoOptionsValues",
  "object">: gremlin "GenericLiteralArgument"]

withOptionKeys :: TypeDefinition
withOptionKeys = define "WithOptionKeys" $ T.union [
  "shortestPath">: gremlin "ShortestPathConstants",
  "connectedComponent">: gremlin "ConnectedComponentConstants",
  "pageRank">: gremlin "PageRankConstants",
  "peerPressure">: gremlin "PeerPressureConstants",
  "io">: gremlin "IoOptionsKeys",
  "withOptionsTokens">: T.unit,
  "withOptionsIndexer">: T.unit]

withOptionsValues :: TypeDefinition
withOptionsValues = define "WithOptionsValues" $ T.enum [
  "tokens",
  "none",
  "ids",
  "labels",
  "keys",
  "values",
  "all",
  "list",
  "map"]

-- Argument types

booleanArgument :: TypeDefinition
booleanArgument = defArgument "BooleanArgument" T.boolean

dateArgument :: TypeDefinition
dateArgument = defArgument "DateArgument" $ gremlin "DateLiteral"

floatArgument :: TypeDefinition
floatArgument = defArgument "FloatArgument" $ gremlin "FloatLiteral"

genericLiteralArgument :: TypeDefinition
genericLiteralArgument = defArgument "GenericLiteralArgument" $ gremlin "GenericLiteral"

genericLiteralListArgument :: TypeDefinition
genericLiteralListArgument = defArgument "GenericLiteralListArgument" $ gremlin "GenericLiteralList"

genericLiteralMapArgument :: TypeDefinition
genericLiteralMapArgument = defArgument "GenericLiteralMapArgument" $ gremlin "GenericLiteralMap"

genericLiteralMapNullableArgument :: TypeDefinition
genericLiteralMapNullableArgument = defArgument "GenericLiteralMapNullableArgument" $ T.optional $ gremlin "GenericLiteralMap"

integerArgument :: TypeDefinition
integerArgument = defArgument "IntegerArgument" $ gremlin "IntegerLiteral"

stringArgument :: TypeDefinition
stringArgument = defArgument "StringArgument" T.string

stringNullableArgument :: TypeDefinition
stringNullableArgument = defArgument "StringNullableArgument" $ T.optional T.string

structureVertexArgument :: TypeDefinition
structureVertexArgument = defArgument "StructureVertexArgument" $ gremlin "StructureVertex"

traversalBiFunctionArgument :: TypeDefinition
traversalBiFunctionArgument = defArgument "TraversalBiFunctionArgument" $ gremlin "TraversalOperator"

traversalCardinalityArgument :: TypeDefinition
traversalCardinalityArgument = defArgument "TraversalCardinalityArgument" $ gremlin "TraversalCardinality"

traversalColumnArgument :: TypeDefinition
traversalColumnArgument = defArgument "TraversalColumnArgument" $ gremlin "TraversalColumn"

traversalComparatorArgument :: TypeDefinition
traversalComparatorArgument = defArgument "TraversalComparatorArgument" $ gremlin "TraversalOrder"

traversalDTArgument :: TypeDefinition
traversalDTArgument = defArgument "TraversalDTArgument" $ gremlin "TraversalDT"

traversalDirectionArgument :: TypeDefinition
traversalDirectionArgument = defArgument "TraversalDirectionArgument" $ gremlin "TraversalDirection"

traversalFunctionArgument :: TypeDefinition
traversalFunctionArgument = defArgument "TraversalFunctionArgument" $ gremlin "TraversalFunction"

traversalMergeArgument :: TypeDefinition
traversalMergeArgument = defArgument "TraversalMergeArgument" $ gremlin "TraversalMerge"

traversalOrderArgument :: TypeDefinition
traversalOrderArgument = defArgument "TraversalOrderArgument" $ gremlin "TraversalOrder"

traversalPopArgument :: TypeDefinition
traversalPopArgument = defArgument "TraversalPopArgument" $ gremlin "TraversalPop"

traversalSackMethodArgument :: TypeDefinition
traversalSackMethodArgument = defArgument "TraversalSackMethodArgument" T.unit

traversalScopeArgument :: TypeDefinition
traversalScopeArgument = defArgument "TraversalScopeArgument" $ gremlin "TraversalScope"

traversalTokenArgument :: TypeDefinition
traversalTokenArgument = defArgument "TraversalTokenArgument" $ gremlin "TraversalToken"

-- Literal types

dateLiteral :: TypeDefinition
dateLiteral = define "DateLiteral" $ T.wrap $ T.optional $ gremlin "StringArgument"

floatLiteral :: TypeDefinition
floatLiteral = define "FloatLiteral" $ T.wrap T.float64

genericLiteral :: TypeDefinition
genericLiteral = define "GenericLiteral" $ T.union [
  "numeric">: gremlin "NumericLiteral",
  "boolean">: T.boolean,
  "string">: T.string,
  "date">: gremlin "DateLiteral",
  "null">: T.unit,
  "nan">: T.unit,
  "inf">: T.unit,
  "traversalToken">: gremlin "TraversalToken",
  "traversalCardinality">: gremlin "TraversalCardinality",
  "traversalDirection">: gremlin "TraversalDirection",
  "traversalMerge">: gremlin "TraversalMerge",
  "traversalPick">: gremlin "TraversalPick",
  "traversalDT">: gremlin "TraversalDT",
  "structureVertex">: gremlin "StructureVertex",
  "genericLiteralSet">: gremlin "GenericLiteralSet",
  "genericLiteralCollection">: gremlin "GenericLiteralCollection",
  "genericLiteralRange">: gremlin "GenericLiteralRange",
  "nestedTraversal">: gremlin "NestedTraversal",
  "terminatedTraversal">: gremlin "TerminatedTraversal",
  "genericLiteralMap">: gremlin "GenericLiteralMap"]

genericLiteralCollection :: TypeDefinition
genericLiteralCollection = define "GenericLiteralCollection" $ T.wrap $ T.list $ gremlin "GenericLiteral"

genericLiteralList :: TypeDefinition
genericLiteralList = define "GenericLiteralList" $ T.wrap $ T.list $ gremlin "GenericLiteral"

genericLiteralMap :: TypeDefinition
genericLiteralMap = define "GenericLiteralMap" $ T.wrap $ T.list $ gremlin "MapEntry"

genericLiteralRange :: TypeDefinition
genericLiteralRange = define "GenericLiteralRange" $ T.union [
  "integer">: gremlin "IntegerRange",
  "string">: gremlin "StringRange"]

genericLiteralSet :: TypeDefinition
genericLiteralSet = define "GenericLiteralSet" $ T.wrap $ T.list $ gremlin "GenericLiteral"

identifier :: TypeDefinition
identifier = define "Identifier" $ T.wrap T.string

integerLiteral :: TypeDefinition
integerLiteral = define "IntegerLiteral" $ T.wrap T.bigint

integerRange :: TypeDefinition
integerRange = define "IntegerRange" $ T.record [
  "left">: gremlin "IntegerLiteral",
  "right">: gremlin "IntegerLiteral"]

keyword :: TypeDefinition
keyword = define "Keyword" $ T.enum [
  "edges",
  "keys",
  "new",
  "values"]

mapEntry :: TypeDefinition
mapEntry = define "MapEntry" $ T.union [
  "key">: gremlin "MapKey",
  "value">: gremlin "GenericLiteral"]

mapKey :: TypeDefinition
mapKey = define "MapKey" $ T.union [
  "string">: T.string,
  "numeric">: gremlin "NumericLiteral",
  "traversalToken">: gremlin "TraversalToken",
  "traversalDirection">: gremlin "TraversalDirection",
  "set">: gremlin "GenericLiteralSet",
  "collection">: gremlin "GenericLiteralCollection",
  "map">: gremlin "GenericLiteralMap",
  "keyword">: gremlin "Keyword",
  "identifier">: gremlin "Identifier"]

numericLiteral :: TypeDefinition
numericLiteral = define "NumericLiteral" $ T.union [
  "integer">: gremlin "IntegerLiteral",
  "float">: gremlin "FloatLiteral"]

stringRange :: TypeDefinition
stringRange = define "StringRange" $ T.record [
  "left">: T.string,
  "right">: T.string]
