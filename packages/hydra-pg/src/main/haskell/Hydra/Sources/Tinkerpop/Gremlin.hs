module Hydra.Sources.Tinkerpop.Gremlin where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
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
            moduleMetadata = descriptionMetadata (Just ("A Gremlin model, based on the Apache TinkerPop Gremlin ANTLR grammar "
      ++ "(version 3.8.1). This is a streamlined model: it preserves the semantics of the grammar "
      ++ "(steps, predicates, arguments, literals) while collapsing pure parser-production artifacts "
      ++ "(single-use 'XAndY'/'XOrY' glue types and left-recursion 'rest' continuations) so that the "
      ++ "representation maps cleanly onto TinkerPop's native traversal model."))}
  where
    definitions = [
      asArgs,
      barrierArgs,
      booleanArgument,
      byArgs,
      byOtherArgs,
      cardinalityAndMap,
      cardinalityAndObjects,
      chainedTraversal,
      chooseArgs,
      concatArgs,
      configuration,
      connectedComponentConstants,
      dateAddArgs,
      dateArgument,
      dateDiffArgs,
      dateLiteral,
      dedupArgs,
      directionAndVarargs,
      floatArgument,
      floatLiteral,
      foldArgs,
      fromArgs,
      genericLiteral,
      genericLiteralArgument,
      genericLiteralCollection,
      genericLiteralList,
      genericLiteralListArgument,
      genericLiteralMap,
      genericLiteralMapArgument,
      genericLiteralMapNullableArgument,
      genericLiteralRange,
      genericLiteralSet,
      hasArgs,
      hasArgsWithKey,
      hasArgsWithToken,
      hasValueClause,
      identifier,
      integerArgument,
      integerLiteral,
      integerRange,
      ioOptionsKeys,
      ioOptionsValues,
      keyword,
      keywordOrIdentifier,
      mapEntry,
      mapKey,
      mergeArgs,
      mergeMapOption,
      mergeTraversalOption,
      nestedTraversal,
      numericArgument,
      numericLiteral,
      objectAndTraversal,
      optionArgs,
      pageRankConstants,
      peerPressureConstants,
      popAndTraversal,
      predicateAndTraversal,
      predicateOrObject,
      predicateOrObjects,
      predicateOrStrings,
      predicateOrTraversal,
      predicateOrTraversalChoice,
      propertyArgs,
      query,
      queryList,
      rangeArgs,
      rangeArgument,
      repeatArgs,
      replaceArgs,
      rootTraversal,
      rootTraversalQuery,
      sampleByScope,
      scopeAndInteger,
      scopeStringArgs,
      selectArgs,
      selectByKeys,
      serviceArguments,
      serviceCall,
      shortestPathConstants,
      splitArgs,
      stringAndObject,
      stringAndOptionalObject,
      stringArgument,
      stringArgumentOrNestedTraversal,
      stringKeyAndObject,
      stringKeyAndPredicate,
      stringNullableArgument,
      stringRange,
      structureVertex,
      structureVertexArgument,
      substringArgs,
      tailArgs,
      terminatedTraversal,
      toArgs,
      transactionPart,
      traversalBiFunctionArgument,
      traversalCardinality,
      traversalCardinalityArgument,
      traversalColumn,
      traversalColumnArgument,
      traversalComparatorArgument,
      traversalDT,
      traversalDTArgument,
      traversalDirection,
      traversalDirectionArgument,
      traversalFunction,
      traversalFunctionArgument,
      traversalGType,
      traversalGTypeArgument,
      traversalMerge,
      traversalMergeArgument,
      traversalMethod,
      traversalOperator,
      traversalOrder,
      traversalOrderArgument,
      traversalPick,
      traversalPop,
      traversalPopArgument,
      traversalPredicate,
      traversalSackMethodArgument,
      traversalScope,
      traversalScopeArgument,
      traversalSource,
      traversalSourceQuery,
      traversalSourceSelfMethod,
      traversalSourceSpawnMethod,
      traversalStrategy,
      traversalTerminalMethod,
      traversalToken,
      traversalTokenArgument,
      twoTraversalPredicates,
      typeOfArg,
      valueMapArgs,
      valueMapBooleanArgs,
      whereArgs,
      whereWithPredicateArgs,
      withArgs,
      withArgsKeys,
      withArgsValues,
      withOptionKeys,
      withOptionsValues,
      withSackArgs]


-- Helper for argument types (value or variable)
defArgument :: String -> Type -> TypeDefinition
defArgument name typ = define name $ T.union [
  "value">: typ,
  "variable">: gremlin "Identifier"]


gremlin :: String -> Type
gremlin = typeref ns

-- Type definitions

asArgs :: TypeDefinition
asArgs = define "AsArgs" $ T.record [
  "first">: gremlin "StringArgument",
  "rest">: T.list $ gremlin "StringNullableArgument"]

barrierArgs :: TypeDefinition
barrierArgs = define "BarrierArgs" $ T.union [
  "consumer">: gremlin "TraversalSackMethodArgument",
  "int">: gremlin "IntegerArgument"]

booleanArgument :: TypeDefinition
booleanArgument = defArgument "BooleanArgument" T.boolean

byArgs :: TypeDefinition
byArgs = define "ByArgs" $ T.union [
  "order">: gremlin "TraversalOrderArgument",
  "token">: gremlin "TraversalTokenArgument",
  "other">: gremlin "ByOtherArgs"]

byOtherArgs :: TypeDefinition
byOtherArgs = define "ByOtherArgs" $ T.union [
  "comparator">: T.optional $ gremlin "TraversalComparatorArgument",
  -- Inlined: was TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
  "function">: gremlin "TraversalFunctionArgument",
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

cardinalityAndMap :: TypeDefinition
cardinalityAndMap = define "CardinalityAndMap" $ T.record [
  "cardinality">: gremlin "TraversalCardinalityArgument",
  "object">: gremlin "GenericLiteralMapNullableArgument"]

cardinalityAndObjects :: TypeDefinition
cardinalityAndObjects = define "CardinalityAndObjects" $ T.record [
  "cardinality">: gremlin "TraversalCardinalityArgument",
  "objects">: minLengthList 2 $ gremlin "GenericLiteralArgument"]

chainedTraversal :: TypeDefinition
chainedTraversal = define "ChainedTraversal" $ T.wrap $ nonemptyList $ gremlin "TraversalMethod"

chooseArgs :: TypeDefinition
chooseArgs = define "ChooseArgs" $ T.union [
  "function">: gremlin "TraversalFunctionArgument",
  -- Inlined PredicateTraversalArgument: predicate + 1..2 nested traversals
  "predicateTraversal">: gremlin "PredicateOrTraversalChoice",
  "traversal">: gremlin "NestedTraversal"]

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

dateArgument :: TypeDefinition
dateArgument = defArgument "DateArgument" $ gremlin "DateLiteral"

dateDiffArgs :: TypeDefinition
dateDiffArgs = define "DateDiffArgs" $ T.union [
  "traversal">: gremlin "NestedTraversal",
  "date">: gremlin "DateArgument"]

dateLiteral :: TypeDefinition
dateLiteral = define "DateLiteral" $ T.wrap $ T.optional $ gremlin "StringArgument"

dedupArgs :: TypeDefinition
dedupArgs = define "DedupArgs" $ T.union [
  "scopeString">: gremlin "ScopeStringArgs",
  "string">: nonemptyList $ gremlin "StringNullableArgument"]

directionAndVarargs :: TypeDefinition
directionAndVarargs = define "DirectionAndVarargs" $ T.record [
  "direction">: gremlin "TraversalDirectionArgument",
  "varargs">: T.list $ gremlin "StringNullableArgument"]

floatArgument :: TypeDefinition
floatArgument = defArgument "FloatArgument" $ gremlin "FloatLiteral"

floatLiteral :: TypeDefinition
floatLiteral = define "FloatLiteral" $ T.union [
  "float">: T.float32,
  "double">: T.float64,
  "big">: T.decimal]

foldArgs :: TypeDefinition
foldArgs = define "FoldArgs" $ T.record [
  "seed">: gremlin "GenericLiteralArgument",
  "biFunction">: gremlin "TraversalBiFunctionArgument"]

fromArgs :: TypeDefinition
fromArgs = define "FromArgs" $ T.union [
  "string">: gremlin "StringArgument",
  "vertex">: gremlin "StructureVertexArgument",
  "traversal">: gremlin "NestedTraversal"]

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
  "traversalGType">: gremlin "TraversalGType",
  "structureVertex">: gremlin "StructureVertex",
  "genericLiteralSet">: gremlin "GenericLiteralSet",
  "genericLiteralCollection">: gremlin "GenericLiteralCollection",
  "genericLiteralRange">: gremlin "GenericLiteralRange",
  "nestedTraversal">: gremlin "NestedTraversal",
  "terminatedTraversal">: gremlin "TerminatedTraversal",
  "genericLiteralMap">: gremlin "GenericLiteralMap"]

genericLiteralArgument :: TypeDefinition
genericLiteralArgument = defArgument "GenericLiteralArgument" $ gremlin "GenericLiteral"

genericLiteralCollection :: TypeDefinition
genericLiteralCollection = define "GenericLiteralCollection" $ T.wrap $ T.list $ gremlin "GenericLiteral"

genericLiteralList :: TypeDefinition
genericLiteralList = define "GenericLiteralList" $ T.wrap $ T.list $ gremlin "GenericLiteral"

genericLiteralListArgument :: TypeDefinition
genericLiteralListArgument = defArgument "GenericLiteralListArgument" $ gremlin "GenericLiteralList"

genericLiteralMap :: TypeDefinition
genericLiteralMap = define "GenericLiteralMap" $ T.wrap $ T.list $ gremlin "MapEntry"

genericLiteralMapArgument :: TypeDefinition
genericLiteralMapArgument = defArgument "GenericLiteralMapArgument" $ gremlin "GenericLiteralMap"

genericLiteralMapNullableArgument :: TypeDefinition
genericLiteralMapNullableArgument = defArgument "GenericLiteralMapNullableArgument" $ T.optional $ gremlin "GenericLiteralMap"

genericLiteralRange :: TypeDefinition
genericLiteralRange = define "GenericLiteralRange" $ T.union [
  "integer">: gremlin "IntegerRange",
  "string">: gremlin "StringRange"]

genericLiteralSet :: TypeDefinition
genericLiteralSet = define "GenericLiteralSet" $ T.wrap $ T.list $ gremlin "GenericLiteral"

hasArgs :: TypeDefinition
hasArgs = define "HasArgs" $ T.union [
  "string">: gremlin "HasArgsWithKey",
  "traversalToken">: gremlin "HasArgsWithToken"]

hasArgsWithKey :: TypeDefinition
hasArgsWithKey = define "HasArgsWithKey" $ T.record [
  "key">: gremlin "StringNullableArgument",
  -- Inlined HasStringArgumentAndOptionalStringLiteralVarargsRest as an optional value clause
  "value">: T.optional $ gremlin "HasValueClause"]

hasArgsWithToken :: TypeDefinition
hasArgsWithToken = define "HasArgsWithToken" $ T.record [
  "token">: gremlin "TraversalTokenArgument",
  "value">: gremlin "HasValueClause"]

-- Unified "has" value clause (object | predicate | traversal | string+object | string+predicate)

hasValueClause :: TypeDefinition
hasValueClause = define "HasValueClause" $ T.union [
  "object">: gremlin "GenericLiteralArgument",
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal",
  "keyObject">: gremlin "StringKeyAndObject",
  "keyPredicate">: gremlin "StringKeyAndPredicate"]

identifier :: TypeDefinition
identifier = define "Identifier" $ T.wrap T.string

integerArgument :: TypeDefinition
integerArgument = defArgument "IntegerArgument" $ gremlin "IntegerLiteral"

integerLiteral :: TypeDefinition
integerLiteral = define "IntegerLiteral" $ T.union [
  "byte">: T.int8,
  "short">: T.int16,
  "int">: T.int32,
  "long">: T.int64,
  "big">: T.bigint]

integerRange :: TypeDefinition
integerRange = define "IntegerRange" $ T.record [
  "left">: gremlin "IntegerLiteral",
  "right">: gremlin "IntegerLiteral"]

ioOptionsKeys :: TypeDefinition
ioOptionsKeys = define "IoOptionsKeys" $ T.enum [
  "reader",
  "writer"]

ioOptionsValues :: TypeDefinition
ioOptionsValues = define "IoOptionsValues" $ T.enum [
  "gryo",
  "graphson",
  "graphml"]

keyword :: TypeDefinition
keyword = define "Keyword" $ T.enum [
  "edges",
  "keys",
  "new",
  "values"]

keywordOrIdentifier :: TypeDefinition
keywordOrIdentifier = define "KeywordOrIdentifier" $ T.union [
  "keyword">: gremlin "Keyword",
  "identifier">: gremlin "Identifier"]

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

mergeArgs :: TypeDefinition
mergeArgs = define "MergeArgs" $ T.union [
  "map">: gremlin "GenericLiteralMapNullableArgument",
  "traversal">: gremlin "NestedTraversal"]

mergeMapOption :: TypeDefinition
mergeMapOption = define "MergeMapOption" $ T.record [
  "merge">: gremlin "TraversalMergeArgument",
  "map">: gremlin "GenericLiteralMapNullableArgument",
  "cardinality">: T.optional $ gremlin "TraversalCardinality"]

mergeTraversalOption :: TypeDefinition
mergeTraversalOption = define "MergeTraversalOption" $ T.record [
  "merge">: gremlin "TraversalMergeArgument",
  "traversal">: gremlin "NestedTraversal"]

nestedTraversal :: TypeDefinition
nestedTraversal = define "NestedTraversal" $ T.union [
  "root">: gremlin "RootTraversal",
  "chained">: gremlin "ChainedTraversal",
  "anonymous">: gremlin "ChainedTraversal"]

-- A chained traversal is simply a non-empty sequence of steps.
-- (Streamlined from the grammar's left-recursive first/rest + ChainedTraversalElement.)

numericArgument :: TypeDefinition
numericArgument = defArgument "NumericArgument" $ gremlin "NumericLiteral"

numericLiteral :: TypeDefinition
numericLiteral = define "NumericLiteral" $ T.union [
  "integer">: gremlin "IntegerLiteral",
  "float">: gremlin "FloatLiteral"]

objectAndTraversal :: TypeDefinition
objectAndTraversal = define "ObjectAndTraversal" $ T.record [
  "object">: gremlin "GenericLiteralArgument",
  "traversal">: gremlin "NestedTraversal"]

optionArgs :: TypeDefinition
optionArgs = define "OptionArgs" $ T.union [
  "predicateTraversal">: gremlin "PredicateAndTraversal",
  "mergeMap">: gremlin "MergeMapOption",
  "mergeTraversal">: gremlin "MergeTraversalOption",
  "objectTraversal">: gremlin "ObjectAndTraversal",
  "traversal">: gremlin "NestedTraversal"]

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

popAndTraversal :: TypeDefinition
popAndTraversal = define "PopAndTraversal" $ T.record [
  "pop">: gremlin "TraversalPopArgument",
  "traversal">: gremlin "NestedTraversal"]

predicateAndTraversal :: TypeDefinition
predicateAndTraversal = define "PredicateAndTraversal" $ T.record [
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

-- choose(predicate, trueTraversal[, falseTraversal])

predicateOrObject :: TypeDefinition
predicateOrObject = define "PredicateOrObject" $ T.union [
  "predicate">: gremlin "TraversalPredicate",
  "object">: gremlin "GenericLiteralArgument"]

predicateOrObjects :: TypeDefinition
predicateOrObjects = define "PredicateOrObjects" $ T.union [
  "predicate">: gremlin "TraversalPredicate",
  "objects">: T.list $ gremlin "GenericLiteralArgument"]

predicateOrStrings :: TypeDefinition
predicateOrStrings = define "PredicateOrStrings" $ T.union [
  "predicate">: gremlin "TraversalPredicate",
  "strings">: nonemptyList $ gremlin "StringNullableArgument"]

predicateOrTraversal :: TypeDefinition
predicateOrTraversal = define "PredicateOrTraversal" $ T.union [
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

predicateOrTraversalChoice :: TypeDefinition
predicateOrTraversalChoice = define "PredicateOrTraversalChoice" $ T.record [
  "predicate">: gremlin "TraversalPredicate",
  "true">: gremlin "NestedTraversal",
  "false">: T.optional $ gremlin "NestedTraversal"]

propertyArgs :: TypeDefinition
propertyArgs = define "PropertyArgs" $ T.union [
  "cardinalityObjects">: gremlin "CardinalityAndObjects",
  "objects">: minLengthList 2 $ gremlin "GenericLiteralArgument",
  "object">: gremlin "GenericLiteralMapNullableArgument",
  "cardinalityObject">: gremlin "CardinalityAndMap"]

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

repeatArgs :: TypeDefinition
repeatArgs = define "RepeatArgs" $ T.record [
  "string">: T.optional $ gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

replaceArgs :: TypeDefinition
replaceArgs = define "ReplaceArgs" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "from">: gremlin "StringNullableArgument",
  "to">: gremlin "StringNullableArgument"]

rootTraversal :: TypeDefinition
rootTraversal = define "RootTraversal" $ T.record [
  "source">: gremlin "TraversalSource",
  "spawnMethod">: gremlin "TraversalSourceSpawnMethod",
  "chained">: T.list $ gremlin "TraversalMethod"]

rootTraversalQuery :: TypeDefinition
rootTraversalQuery = define "RootTraversalQuery" $ T.record [
  "root">: gremlin "RootTraversal",
  "terminalMethod">: T.optional $ gremlin "TraversalTerminalMethod"]

sampleByScope :: TypeDefinition
sampleByScope = define "SampleByScope" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "integer">: gremlin "IntegerArgument"]

scopeAndInteger :: TypeDefinition
scopeAndInteger = define "ScopeAndInteger" $ T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "integer">: gremlin "IntegerArgument"]

scopeStringArgs :: TypeDefinition
scopeStringArgs = define "ScopeStringArgs" $ T.record [
  "scope">: gremlin "TraversalScopeArgument",
  "strings">: T.list $ gremlin "StringNullableArgument"]

selectArgs :: TypeDefinition
selectArgs = define "SelectArgs" $ T.union [
  "column">: gremlin "TraversalColumnArgument",
  "popStrings">: gremlin "SelectByKeys",
  "popTraversal">: gremlin "PopAndTraversal",
  "strings">: nonemptyList $ gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

selectByKeys :: TypeDefinition
selectByKeys = define "SelectByKeys" $ T.record [
  "pop">: gremlin "TraversalPopArgument",
  "keys">: nonemptyList $ gremlin "StringArgument"]

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

stringAndObject :: TypeDefinition
stringAndObject = define "StringAndObject" $ T.record [
  "key">: gremlin "StringArgument",
  "value">: gremlin "GenericLiteralArgument"]

stringAndOptionalObject :: TypeDefinition
stringAndOptionalObject = define "StringAndOptionalObject" $ T.record [
  "key">: gremlin "StringArgument",
  "value">: T.optional $ gremlin "GenericLiteralArgument"]

stringArgument :: TypeDefinition
stringArgument = defArgument "StringArgument" T.string

stringArgumentOrNestedTraversal :: TypeDefinition
stringArgumentOrNestedTraversal = define "StringArgumentOrNestedTraversal" $ T.union [
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

stringKeyAndObject :: TypeDefinition
stringKeyAndObject = define "StringKeyAndObject" $ T.record [
  "key">: gremlin "StringNullableArgument",
  "object">: gremlin "GenericLiteralArgument"]

stringKeyAndPredicate :: TypeDefinition
stringKeyAndPredicate = define "StringKeyAndPredicate" $ T.record [
  "key">: gremlin "StringNullableArgument",
  "predicate">: gremlin "TraversalPredicate"]

stringNullableArgument :: TypeDefinition
stringNullableArgument = defArgument "StringNullableArgument" $ T.optional T.string

stringRange :: TypeDefinition
stringRange = define "StringRange" $ T.record [
  "left">: T.string,
  "right">: T.string]

structureVertex :: TypeDefinition
structureVertex = define "StructureVertex" $ T.record [
  "new">: T.boolean,
  "id">: gremlin "GenericLiteralArgument",
  "label">: gremlin "StringArgument"]

structureVertexArgument :: TypeDefinition
structureVertexArgument = defArgument "StructureVertexArgument" $ gremlin "StructureVertex"

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

traversalBiFunctionArgument :: TypeDefinition
traversalBiFunctionArgument = defArgument "TraversalBiFunctionArgument" $ gremlin "TraversalOperator"

traversalCardinality :: TypeDefinition
traversalCardinality = define "TraversalCardinality" $ T.union [
  "single">: gremlin "GenericLiteral",
  "set">: gremlin "GenericLiteral",
  "list">: gremlin "GenericLiteral"]

traversalCardinalityArgument :: TypeDefinition
traversalCardinalityArgument = defArgument "TraversalCardinalityArgument" $ gremlin "TraversalCardinality"

traversalColumn :: TypeDefinition
traversalColumn = define "TraversalColumn" $ T.enum [
  "keys",
  "values"]

traversalColumnArgument :: TypeDefinition
traversalColumnArgument = defArgument "TraversalColumnArgument" $ gremlin "TraversalColumn"

traversalComparatorArgument :: TypeDefinition
traversalComparatorArgument = defArgument "TraversalComparatorArgument" $ gremlin "TraversalOrder"

traversalDirection :: TypeDefinition
traversalDirection = define "TraversalDirection" $ T.enum [
  "in",
  "out",
  "both",
  "from",
  "to"]

traversalDirectionArgument :: TypeDefinition
traversalDirectionArgument = defArgument "TraversalDirectionArgument" $ gremlin "TraversalDirection"

traversalDT :: TypeDefinition
traversalDT = define "TraversalDT" $ T.enum [
  "second",
  "minute",
  "hour",
  "day"]

traversalDTArgument :: TypeDefinition
traversalDTArgument = defArgument "TraversalDTArgument" $ gremlin "TraversalDT"

traversalFunction :: TypeDefinition
traversalFunction = define "TraversalFunction" $ T.union [
  "token">: gremlin "TraversalToken",
  "column">: gremlin "TraversalColumn"]

traversalFunctionArgument :: TypeDefinition
traversalFunctionArgument = defArgument "TraversalFunctionArgument" $ gremlin "TraversalFunction"

traversalGType :: TypeDefinition
traversalGType = define "TraversalGType" $ T.enum [
  "bigDecimal", "bigDecimalU",
  "bigInt", "bigIntU",
  "binary", "binaryU",
  "boolean", "booleanU",
  "byte", "byteU",
  "char", "charU",
  "dateTime", "dateTimeU",
  "double", "doubleU",
  "duration", "durationU",
  "edge", "edgeU",
  "float", "floatU",
  "graph", "graphU",
  "int", "intU",
  "list", "listU",
  "long", "longU",
  "map", "mapU",
  "null", "nullU",
  "number", "numberU",
  "path", "pathU",
  "property", "propertyU",
  "set", "setU",
  "short", "shortU",
  "string", "stringU",
  "tree", "treeU",
  "uuid", "uuidL",
  "vertex", "vertexU",
  "vproperty", "vpropertyU"]

traversalGTypeArgument :: TypeDefinition
traversalGTypeArgument = defArgument "TraversalGTypeArgument" $ gremlin "TraversalGType"

traversalMerge :: TypeDefinition
traversalMerge = define "TraversalMerge" $ T.enum [
  "onCreate",
  "onMatch",
  "outV",
  "inV"]

traversalMergeArgument :: TypeDefinition
traversalMergeArgument = defArgument "TraversalMergeArgument" $ gremlin "TraversalMerge"

traversalMethod :: TypeDefinition
traversalMethod = define "TraversalMethod" $ T.union [
  "v">: nonemptyList $ gremlin "GenericLiteralArgument",
  "e">: nonemptyList $ gremlin "GenericLiteralArgument",
  "addE">: gremlin "StringArgumentOrNestedTraversal",
  "addV">: T.optional $ gremlin "StringArgumentOrNestedTraversal",
  "mergeE">: gremlin "MergeArgs",
  "mergeV">: gremlin "MergeArgs",
  "aggregate">: gremlin "StringArgument",
  "all">: gremlin "TraversalPredicate",
  "and">: T.list $ gremlin "NestedTraversal",
  "any">: gremlin "TraversalPredicate",
  "as">: gremlin "AsArgs",
  "asBool">: T.unit,
  "asDate">: T.unit,
  "asNumber">: T.optional $ gremlin "TraversalGTypeArgument",
  "asString">: T.optional $ gremlin "TraversalScopeArgument",
  "barrier">: T.optional $ gremlin "BarrierArgs",
  "both">: nonemptyList $ gremlin "StringNullableArgument",
  "bothE">: nonemptyList $ gremlin "StringNullableArgument",
  "bothV">: T.unit,
  "branch">: gremlin "NestedTraversal",
  "by">: gremlin "ByArgs",
  "call">: gremlin "ServiceCall",
  "cap">: gremlin "AsArgs",
  "choose">: gremlin "ChooseArgs",
  "coalesce">: T.list $ gremlin "NestedTraversal",
  "coin">: gremlin "NumericArgument",
  "combine">: gremlin "GenericLiteralArgument",
  "concat">: gremlin "ConcatArgs",
  "conjoin">: gremlin "StringArgument",
  "connectedComponent">: T.unit,
  "constant">: gremlin "GenericLiteralArgument",
  "count">: T.optional $ gremlin "TraversalScopeArgument",
  "cyclicPath">: T.unit,
  "dateAdd">: gremlin "DateAddArgs",
  "dateDiff">: gremlin "DateDiffArgs",
  "dedup">: gremlin "DedupArgs",
  "difference">: gremlin "GenericLiteralArgument",
  "discard">: T.unit,
  "disjunct">: gremlin "GenericLiteralArgument",
  "drop">: T.unit,
  "element">: nonemptyList $ gremlin "StringNullableArgument",
  "elementMap">: nonemptyList $ gremlin "StringNullableArgument",
  "emit">: T.optional $ gremlin "PredicateOrTraversal",
  "fail">: T.optional $ gremlin "StringArgument",
  "filter">: gremlin "PredicateOrTraversal",
  "flatMap">: gremlin "NestedTraversal",
  "fold">: T.optional $ gremlin "FoldArgs",
  "format">: gremlin "StringArgument",
  "from">: gremlin "FromArgs",
  "group">: T.optional $ gremlin "StringArgument",
  "groupCount">: T.optional $ gremlin "StringArgument",
  "has">: gremlin "HasArgs",
  "hasId">: gremlin "PredicateOrObjects",
  "hasKey">: gremlin "PredicateOrStrings",
  "hasLabel">: gremlin "PredicateOrStrings",
  "hasNot">: gremlin "StringNullableArgument",
  "hasValue">: gremlin "PredicateOrObjects",
  "id">: T.unit,
  "identity">: T.unit,
  "in">: nonemptyList $ gremlin "StringNullableArgument",
  "inE">: nonemptyList $ gremlin "StringNullableArgument",
  "inV">: T.unit,
  "index">: T.unit,
  "inject">: nonemptyList $ gremlin "GenericLiteralArgument",
  "intersect">: gremlin "GenericLiteralArgument",
  "is">: gremlin "PredicateOrObject",
  "key">: T.unit,
  "label">: T.unit,
  "length">: T.optional $ gremlin "TraversalScopeArgument",
  "limit">: gremlin "ScopeAndInteger",
  "local">: gremlin "NestedTraversal",
  "loops">: T.optional $ gremlin "StringArgument",
  "lTrim">: T.optional $ gremlin "TraversalScopeArgument",
  "map">: gremlin "NestedTraversal",
  "match">: T.list $ gremlin "NestedTraversal",
  "math">: gremlin "StringArgument",
  "max">: T.optional $ gremlin "TraversalScopeArgument",
  "mean">: T.optional $ gremlin "TraversalScopeArgument",
  "merge">: gremlin "GenericLiteralArgument",
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
  "pageRank">: T.optional $ gremlin "NumericArgument",
  "path">: T.unit,
  "peerPressure">: T.unit,
  "product">: gremlin "GenericLiteralArgument",
  "profile">: T.optional $ gremlin "StringArgument",
  "project">: gremlin "AsArgs",
  "properties">: nonemptyList $ gremlin "StringNullableArgument",
  "property">: gremlin "PropertyArgs",
  "propertyMap">: nonemptyList $ gremlin "StringNullableArgument",
  "range">: gremlin "RangeArgs",
  "read">: T.unit,
  "repeat">: gremlin "RepeatArgs",
  "replace">: gremlin "ReplaceArgs",
  "reverse">: T.unit,
  "rTrim">: T.optional $ gremlin "TraversalScopeArgument",
  "sack">: T.optional $ gremlin "TraversalBiFunctionArgument",
  "sample">: gremlin "SampleByScope",
  "select">: gremlin "SelectArgs",
  "shortestPath">: T.unit,
  "sideEffect">: gremlin "NestedTraversal",
  "simplePath">: T.unit,
  "skip">: gremlin "ScopeAndInteger",
  "split">: gremlin "SplitArgs",
  "subgraph">: gremlin "StringArgument",
  "substring">: gremlin "SubstringArgs",
  "sum">: T.optional $ gremlin "TraversalScopeArgument",
  "tail">: T.optional $ gremlin "TailArgs",
  "timeLimit">: gremlin "IntegerArgument",
  "times">: gremlin "IntegerArgument",
  "to">: gremlin "ToArgs",
  "toE">: gremlin "DirectionAndVarargs",
  "toLower">: T.optional $ gremlin "TraversalScopeArgument",
  "toUpper">: T.optional $ gremlin "TraversalScopeArgument",
  "toV">: gremlin "TraversalDirectionArgument",
  "tree">: T.optional $ gremlin "StringArgument",
  "trim">: T.optional $ gremlin "TraversalScopeArgument",
  "unfold">: T.unit,
  "union">: T.list $ gremlin "NestedTraversal",
  "until">: gremlin "PredicateOrTraversal",
  "value">: T.unit,
  "valueMap">: gremlin "ValueMapArgs",
  "values">: nonemptyList $ gremlin "StringNullableArgument",
  "where">: gremlin "WhereArgs",
  "with">: gremlin "WithArgs",
  "write">: T.unit]

-- as(label, ...labels) and the structurally-identical cap/project

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
  "asc",
  "desc",
  "shuffle"]

traversalOrderArgument :: TypeDefinition
traversalOrderArgument = defArgument "TraversalOrderArgument" $ gremlin "TraversalOrder"

traversalPick :: TypeDefinition
traversalPick = define "TraversalPick" $ T.enum [
  "any",
  "none",
  "unproductive"]

traversalPop :: TypeDefinition
traversalPop = define "TraversalPop" $ T.enum [
  "first",
  "last",
  "all",
  "mixed"]

traversalPopArgument :: TypeDefinition
traversalPopArgument = defArgument "TraversalPopArgument" $ gremlin "TraversalPop"

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
  "typeOf">: gremlin "TypeOfArg",
  "and">: gremlin "TwoTraversalPredicates",
  "or">: gremlin "TwoTraversalPredicates",
  "negate">: gremlin "TraversalPredicate"]

traversalSackMethodArgument :: TypeDefinition
traversalSackMethodArgument = defArgument "TraversalSackMethodArgument" T.unit

traversalScope :: TypeDefinition
traversalScope = define "TraversalScope" $ T.enum [
  "local",
  "global"]

traversalScopeArgument :: TypeDefinition
traversalScopeArgument = defArgument "TraversalScopeArgument" $ gremlin "TraversalScope"

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
  "withSack">: gremlin "WithSackArgs",
  "withSideEffect">: gremlin "StringAndObject",
  "withStrategies">: nonemptyList $ gremlin "TraversalStrategy",
  "withoutStrategies">: nonemptyList $ gremlin "Identifier",
  "with">: gremlin "StringAndOptionalObject"]

traversalSourceSpawnMethod :: TypeDefinition
traversalSourceSpawnMethod = define "TraversalSourceSpawnMethod" $ T.union [
  "addE">: gremlin "StringArgumentOrNestedTraversal",
  "addV">: T.optional $ gremlin "StringArgumentOrNestedTraversal",
  "e">: nonemptyList $ gremlin "GenericLiteralArgument",
  "v">: nonemptyList $ gremlin "GenericLiteralArgument",
  "mergeV">: gremlin "MergeArgs",
  "mergeE">: gremlin "MergeArgs",
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

traversalTokenArgument :: TypeDefinition
traversalTokenArgument = defArgument "TraversalTokenArgument" $ gremlin "TraversalToken"

-- Literal types

twoTraversalPredicates :: TypeDefinition
twoTraversalPredicates = define "TwoTraversalPredicates" $ T.record [
  "left">: gremlin "TraversalPredicate",
  "right">: gremlin "TraversalPredicate"]

typeOfArg :: TypeDefinition
typeOfArg = define "TypeOfArg" $ T.union [
  "gType">: gremlin "TraversalGType",
  "string">: gremlin "StringArgument"]

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

withSackArgs :: TypeDefinition
withSackArgs = define "WithSackArgs" $ T.record [
  "initialValue">: gremlin "GenericLiteralArgument",
  "biFunction">: T.optional $ gremlin "TraversalBiFunctionArgument"]
