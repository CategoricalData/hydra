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

-- Helper for argument types (value or variable), with a description
defArgumentDoc :: String -> String -> Type -> TypeDefinition
defArgumentDoc name description typ = define name $
  doc description $
  T.union [
  "value">: typ,
  "variable">: gremlin "Identifier"]


gremlin :: String -> Type
gremlin = typeref ns

-- Type definitions

asArgs :: TypeDefinition
asArgs = define "AsArgs" $
  doc "The arguments of a Gremlin as() step" $
  T.record [
  "first">: gremlin "StringArgument",
  "rest">: T.list $ gremlin "StringNullableArgument"]

barrierArgs :: TypeDefinition
barrierArgs = define "BarrierArgs" $
  doc "The arguments of a Gremlin barrier() step" $
  T.union [
  "consumer">: gremlin "TraversalSackMethodArgument",
  "int">: gremlin "IntegerArgument"]

booleanArgument :: TypeDefinition
booleanArgument = defArgumentDoc "BooleanArgument" "A Gremlin boolean argument: a literal value or a bound variable" T.boolean

byArgs :: TypeDefinition
byArgs = define "ByArgs" $
  doc "The arguments of a Gremlin by() modulator" $
  T.union [
  "order">: gremlin "TraversalOrderArgument",
  "token">: gremlin "TraversalTokenArgument",
  "other">: gremlin "ByOtherArgs"]

byOtherArgs :: TypeDefinition
byOtherArgs = define "ByOtherArgs" $
  doc "The arguments of a Gremlin by() modulator taking another traversal or comparator" $
  T.union [
  "comparator">: T.optional $ gremlin "TraversalComparatorArgument",
  -- Inlined: was TraversalFunctionArgumentOrStringArgumentOrNestedTraversal
  "function">: gremlin "TraversalFunctionArgument",
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

cardinalityAndMap :: TypeDefinition
cardinalityAndMap = define "CardinalityAndMap" $
  doc "A cardinality value together with a property map, as used in a merge step" $
  T.record [
  "cardinality">: gremlin "TraversalCardinalityArgument",
  "object">: gremlin "GenericLiteralMapNullableArgument"]

cardinalityAndObjects :: TypeDefinition
cardinalityAndObjects = define "CardinalityAndObjects" $
  doc "A cardinality value together with key-value object pairs, as used in a property() step" $
  T.record [
  "cardinality">: gremlin "TraversalCardinalityArgument",
  "objects">: minLengthList 2 $ gremlin "GenericLiteralArgument"]

chainedTraversal :: TypeDefinition
chainedTraversal = define "ChainedTraversal" $
  doc "A Gremlin traversal expressed as a chain of method calls" $
  T.wrap $ nonemptyList $ gremlin "TraversalMethod"

chooseArgs :: TypeDefinition
chooseArgs = define "ChooseArgs" $
  doc "The arguments of a Gremlin choose() step" $
  T.union [
  "function">: gremlin "TraversalFunctionArgument",
  -- Inlined PredicateTraversalArgument: predicate + 1..2 nested traversals
  "predicateTraversal">: gremlin "PredicateOrTraversalChoice",
  "traversal">: gremlin "NestedTraversal"]

concatArgs :: TypeDefinition
concatArgs = define "ConcatArgs" $
  doc "The arguments of a Gremlin concat() step" $
  T.union [
  "traversal">: nonemptyList $ gremlin "NestedTraversal",
  "string">: nonemptyList $ gremlin "StringNullableArgument"]

configuration :: TypeDefinition
configuration = define "Configuration" $
  doc "A Gremlin configuration map" $
  T.record [
  "key">: gremlin "KeywordOrIdentifier",
  "value">: gremlin "GenericLiteralArgument"]

connectedComponentConstants :: TypeDefinition
connectedComponentConstants = define "ConnectedComponentConstants" $
  doc "The named constants used by the Gremlin connectedComponent() step" $
  T.enum [
  "component",
  "edges",
  "propertyName"]

dateAddArgs :: TypeDefinition
dateAddArgs = define "DateAddArgs" $
  doc "The arguments of a Gremlin dateAdd() step" $
  T.record [
  "unit">: gremlin "TraversalDTArgument",
  "duration">: gremlin "IntegerArgument"]

dateArgument :: TypeDefinition
dateArgument = defArgumentDoc "DateArgument" "A Gremlin date argument: a literal value or a bound variable" $
  gremlin "DateLiteral"

dateDiffArgs :: TypeDefinition
dateDiffArgs = define "DateDiffArgs" $
  doc "The arguments of a Gremlin dateDiff() step" $
  T.union [
  "traversal">: gremlin "NestedTraversal",
  "date">: gremlin "DateArgument"]

dateLiteral :: TypeDefinition
dateLiteral = define "DateLiteral" $
  doc "A Gremlin date literal" $
  T.wrap $ T.optional $ gremlin "StringArgument"

dedupArgs :: TypeDefinition
dedupArgs = define "DedupArgs" $
  doc "The arguments of a Gremlin dedup() step" $
  T.union [
  "scopeString">: gremlin "ScopeStringArgs",
  "string">: nonemptyList $ gremlin "StringNullableArgument"]

directionAndVarargs :: TypeDefinition
directionAndVarargs = define "DirectionAndVarargs" $
  doc "A direction argument together with a variadic list of further arguments" $
  T.record [
  "direction">: gremlin "TraversalDirectionArgument",
  "varargs">: T.list $ gremlin "StringNullableArgument"]

floatArgument :: TypeDefinition
floatArgument = defArgumentDoc "FloatArgument" "A Gremlin floating-point argument: a literal value or a bound variable" $
  gremlin "FloatLiteral"

floatLiteral :: TypeDefinition
floatLiteral = define "FloatLiteral" $
  doc "A Gremlin floating-point literal" $
  T.union [
  "float">: T.float32,
  "double">: T.float64,
  "big">: T.decimal]

foldArgs :: TypeDefinition
foldArgs = define "FoldArgs" $
  doc "The arguments of a Gremlin fold() step" $
  T.record [
  "seed">: gremlin "GenericLiteralArgument",
  "biFunction">: gremlin "TraversalBiFunctionArgument"]

fromArgs :: TypeDefinition
fromArgs = define "FromArgs" $
  doc "The arguments of a Gremlin from() modulator" $
  T.union [
  "string">: gremlin "StringArgument",
  "vertex">: gremlin "StructureVertexArgument",
  "traversal">: gremlin "NestedTraversal"]

genericLiteral :: TypeDefinition
genericLiteral = define "GenericLiteral" $
  doc "A Gremlin generic literal: any of the supported literal forms" $
  T.union [
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
genericLiteralArgument = defArgumentDoc "GenericLiteralArgument" "A Gremlin generic-literal argument: a literal value or a bound variable" $
  gremlin "GenericLiteral"

genericLiteralCollection :: TypeDefinition
genericLiteralCollection = define "GenericLiteralCollection" $
  doc "A Gremlin literal collection: a list, set, or map literal" $
  T.wrap $ T.list $ gremlin "GenericLiteral"

genericLiteralList :: TypeDefinition
genericLiteralList = define "GenericLiteralList" $
  doc "A Gremlin list literal" $
  T.wrap $ T.list $ gremlin "GenericLiteral"

genericLiteralListArgument :: TypeDefinition
genericLiteralListArgument = defArgumentDoc "GenericLiteralListArgument" "A Gremlin list-literal argument: a literal value or a bound variable" $
  gremlin "GenericLiteralList"

genericLiteralMap :: TypeDefinition
genericLiteralMap = define "GenericLiteralMap" $
  doc "A Gremlin map literal" $
  T.wrap $ T.list $ gremlin "MapEntry"

genericLiteralMapArgument :: TypeDefinition
genericLiteralMapArgument = defArgumentDoc "GenericLiteralMapArgument" "A Gremlin map-literal argument: a literal value or a bound variable" $
  gremlin "GenericLiteralMap"

genericLiteralMapNullableArgument :: TypeDefinition
genericLiteralMapNullableArgument = defArgumentDoc "GenericLiteralMapNullableArgument" "A Gremlin optional map-literal argument: a literal value, a bound variable, or null" $
  T.optional $ gremlin "GenericLiteralMap"

genericLiteralRange :: TypeDefinition
genericLiteralRange = define "GenericLiteralRange" $
  doc "A Gremlin literal range expression" $
  T.union [
  "integer">: gremlin "IntegerRange",
  "string">: gremlin "StringRange"]

genericLiteralSet :: TypeDefinition
genericLiteralSet = define "GenericLiteralSet" $
  doc "A Gremlin set literal" $
  T.wrap $ T.list $ gremlin "GenericLiteral"

hasArgs :: TypeDefinition
hasArgs = define "HasArgs" $
  doc "The arguments of a Gremlin has() step" $
  T.union [
  "string">: gremlin "HasArgsWithKey",
  "traversalToken">: gremlin "HasArgsWithToken"]

hasArgsWithKey :: TypeDefinition
hasArgsWithKey = define "HasArgsWithKey" $
  doc "The arguments of a Gremlin has() step qualified by a property key" $
  T.record [
  "key">: gremlin "StringNullableArgument",
  -- Inlined HasStringArgumentAndOptionalStringLiteralVarargsRest as an optional value clause
  "value">: T.optional $ gremlin "HasValueClause"]

hasArgsWithToken :: TypeDefinition
hasArgsWithToken = define "HasArgsWithToken" $
  doc "The arguments of a Gremlin has() step qualified by a traversal token" $
  T.record [
  "token">: gremlin "TraversalTokenArgument",
  "value">: gremlin "HasValueClause"]

-- Unified "has" value clause (object | predicate | traversal | string+object | string+predicate)

hasValueClause :: TypeDefinition
hasValueClause = define "HasValueClause" $
  doc "The value-matching clause of a Gremlin has() step" $
  T.union [
  "object">: gremlin "GenericLiteralArgument",
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal",
  "keyObject">: gremlin "StringKeyAndObject",
  "keyPredicate">: gremlin "StringKeyAndPredicate"]

identifier :: TypeDefinition
identifier = define "Identifier" $
  doc "A Gremlin identifier" $
  T.wrap T.string

integerArgument :: TypeDefinition
integerArgument = defArgumentDoc "IntegerArgument" "A Gremlin integer argument: a literal value or a bound variable" $
  gremlin "IntegerLiteral"

integerLiteral :: TypeDefinition
integerLiteral = define "IntegerLiteral" $
  doc "A Gremlin integer literal" $
  T.union [
  "byte">: T.int8,
  "short">: T.int16,
  "int">: T.int32,
  "long">: T.int64,
  "big">: T.bigint]

integerRange :: TypeDefinition
integerRange = define "IntegerRange" $
  doc "A Gremlin integer range expression" $
  T.record [
  "left">: gremlin "IntegerLiteral",
  "right">: gremlin "IntegerLiteral"]

ioOptionsKeys :: TypeDefinition
ioOptionsKeys = define "IoOptionsKeys" $
  doc "The named keys accepted by a Gremlin io() step's options" $
  T.enum [
  "reader",
  "writer"]

ioOptionsValues :: TypeDefinition
ioOptionsValues = define "IoOptionsValues" $
  doc "The named values accepted by a Gremlin io() step's options" $
  T.enum [
  "gryo",
  "graphson",
  "graphml"]

keyword :: TypeDefinition
keyword = define "Keyword" $
  doc "A reserved Gremlin keyword" $
  T.enum [
  "edges",
  "keys",
  "new",
  "values"]

keywordOrIdentifier :: TypeDefinition
keywordOrIdentifier = define "KeywordOrIdentifier" $
  doc "A Gremlin keyword or a plain identifier" $
  T.union [
  "keyword">: gremlin "Keyword",
  "identifier">: gremlin "Identifier"]

mapEntry :: TypeDefinition
mapEntry = define "MapEntry" $
  doc "A key-value entry in a Gremlin map literal" $
  T.union [
  "key">: gremlin "MapKey",
  "value">: gremlin "GenericLiteral"]

mapKey :: TypeDefinition
mapKey = define "MapKey" $
  doc "The key of a Gremlin map entry" $
  T.union [
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
mergeArgs = define "MergeArgs" $
  doc "The arguments of a Gremlin mergeV()/mergeE() step" $
  T.union [
  "map">: gremlin "GenericLiteralMapNullableArgument",
  "traversal">: gremlin "NestedTraversal"]

mergeMapOption :: TypeDefinition
mergeMapOption = define "MergeMapOption" $
  doc "A merge option expressed as a map" $
  T.record [
  "merge">: gremlin "TraversalMergeArgument",
  "map">: gremlin "GenericLiteralMapNullableArgument",
  "cardinality">: T.optional $ gremlin "TraversalCardinality"]

mergeTraversalOption :: TypeDefinition
mergeTraversalOption = define "MergeTraversalOption" $
  doc "A merge option expressed as a nested traversal" $
  T.record [
  "merge">: gremlin "TraversalMergeArgument",
  "traversal">: gremlin "NestedTraversal"]

nestedTraversal :: TypeDefinition
nestedTraversal = define "NestedTraversal" $
  doc "A traversal nested within another traversal step's arguments" $
  T.union [
  "root">: gremlin "RootTraversal",
  "chained">: gremlin "ChainedTraversal",
  "anonymous">: gremlin "ChainedTraversal"]

-- A chained traversal is simply a non-empty sequence of steps.
-- (Streamlined from the grammar's left-recursive first/rest + ChainedTraversalElement.)

numericArgument :: TypeDefinition
numericArgument = defArgumentDoc "NumericArgument" "A Gremlin numeric argument: a literal value or a bound variable" $
  gremlin "NumericLiteral"

numericLiteral :: TypeDefinition
numericLiteral = define "NumericLiteral" $
  doc "A Gremlin numeric literal: an integer or a float" $
  T.union [
  "integer">: gremlin "IntegerLiteral",
  "float">: gremlin "FloatLiteral"]

objectAndTraversal :: TypeDefinition
objectAndTraversal = define "ObjectAndTraversal" $
  doc "An object argument together with a nested traversal" $
  T.record [
  "object">: gremlin "GenericLiteralArgument",
  "traversal">: gremlin "NestedTraversal"]

optionArgs :: TypeDefinition
optionArgs = define "OptionArgs" $
  doc "The arguments of a Gremlin option() step" $
  T.union [
  "predicateTraversal">: gremlin "PredicateAndTraversal",
  "mergeMap">: gremlin "MergeMapOption",
  "mergeTraversal">: gremlin "MergeTraversalOption",
  "objectTraversal">: gremlin "ObjectAndTraversal",
  "traversal">: gremlin "NestedTraversal"]

pageRankConstants :: TypeDefinition
pageRankConstants = define "PageRankConstants" $
  doc "The named constants used by the Gremlin pageRank() step" $
  T.enum [
  "edges",
  "times",
  "propertyName"]

peerPressureConstants :: TypeDefinition
peerPressureConstants = define "PeerPressureConstants" $
  doc "The named constants used by the Gremlin peerPressure() step" $
  T.enum [
  "edges",
  "times",
  "propertyName"]

popAndTraversal :: TypeDefinition
popAndTraversal = define "PopAndTraversal" $
  doc "A Pop value together with a nested traversal" $
  T.record [
  "pop">: gremlin "TraversalPopArgument",
  "traversal">: gremlin "NestedTraversal"]

predicateAndTraversal :: TypeDefinition
predicateAndTraversal = define "PredicateAndTraversal" $
  doc "A predicate together with a nested traversal" $
  T.record [
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

-- choose(predicate, trueTraversal[, falseTraversal])

predicateOrObject :: TypeDefinition
predicateOrObject = define "PredicateOrObject" $
  doc "A traversal predicate or a plain object argument" $
  T.union [
  "predicate">: gremlin "TraversalPredicate",
  "object">: gremlin "GenericLiteralArgument"]

predicateOrObjects :: TypeDefinition
predicateOrObjects = define "PredicateOrObjects" $
  doc "A traversal predicate or a variadic list of object arguments" $
  T.union [
  "predicate">: gremlin "TraversalPredicate",
  "objects">: T.list $ gremlin "GenericLiteralArgument"]

predicateOrStrings :: TypeDefinition
predicateOrStrings = define "PredicateOrStrings" $
  doc "A traversal predicate or a variadic list of string arguments" $
  T.union [
  "predicate">: gremlin "TraversalPredicate",
  "strings">: nonemptyList $ gremlin "StringNullableArgument"]

predicateOrTraversal :: TypeDefinition
predicateOrTraversal = define "PredicateOrTraversal" $
  doc "A traversal predicate or a nested traversal" $
  T.union [
  "predicate">: gremlin "TraversalPredicate",
  "traversal">: gremlin "NestedTraversal"]

predicateOrTraversalChoice :: TypeDefinition
predicateOrTraversalChoice = define "PredicateOrTraversalChoice" $
  doc "A choice between a traversal predicate and a nested traversal, used in filter steps" $
  T.record [
  "predicate">: gremlin "TraversalPredicate",
  "true">: gremlin "NestedTraversal",
  "false">: T.optional $ gremlin "NestedTraversal"]

propertyArgs :: TypeDefinition
propertyArgs = define "PropertyArgs" $
  doc "The arguments of a Gremlin property() step" $
  T.union [
  "cardinalityObjects">: gremlin "CardinalityAndObjects",
  "objects">: minLengthList 2 $ gremlin "GenericLiteralArgument",
  "object">: gremlin "GenericLiteralMapNullableArgument",
  "cardinalityObject">: gremlin "CardinalityAndMap"]

query :: TypeDefinition
query = define "Query" $
  doc "A Gremlin query" $
  T.union [
  "traversalSource">: gremlin "TraversalSourceQuery",
  "rootTraversal">: gremlin "RootTraversalQuery",
  "toString">: T.unit,
  "empty">: T.unit]

queryList :: TypeDefinition
queryList = define "QueryList" $
  doc "A sequence of Gremlin queries" $
  T.wrap $ nonemptyList $ gremlin "Query"

rangeArgs :: TypeDefinition
rangeArgs = define "RangeArgs" $
  doc "The arguments of a Gremlin range() step" $
  T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "min">: gremlin "IntegerArgument",
  "max">: gremlin "IntegerArgument"]

rangeArgument :: TypeDefinition
rangeArgument = define "RangeArgument" $
  doc "A Gremlin range-valued argument" $
  T.record [
  "min">: gremlin "GenericLiteralArgument",
  "max">: gremlin "GenericLiteralArgument"]

repeatArgs :: TypeDefinition
repeatArgs = define "RepeatArgs" $
  doc "The arguments of a Gremlin repeat() step" $
  T.record [
  "string">: T.optional $ gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

replaceArgs :: TypeDefinition
replaceArgs = define "ReplaceArgs" $
  doc "The arguments of a Gremlin replace() step" $
  T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "from">: gremlin "StringNullableArgument",
  "to">: gremlin "StringNullableArgument"]

rootTraversal :: TypeDefinition
rootTraversal = define "RootTraversal" $
  doc "A Gremlin root-level traversal expression" $
  T.record [
  "source">: gremlin "TraversalSource",
  "spawnMethod">: gremlin "TraversalSourceSpawnMethod",
  "chained">: T.list $ gremlin "TraversalMethod"]

rootTraversalQuery :: TypeDefinition
rootTraversalQuery = define "RootTraversalQuery" $
  doc "A query rooted at a Gremlin traversal source" $
  T.record [
  "root">: gremlin "RootTraversal",
  "terminalMethod">: T.optional $ gremlin "TraversalTerminalMethod"]

sampleByScope :: TypeDefinition
sampleByScope = define "SampleByScope" $
  doc "The scope argument of a Gremlin sample() step" $
  T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "integer">: gremlin "IntegerArgument"]

scopeAndInteger :: TypeDefinition
scopeAndInteger = define "ScopeAndInteger" $
  doc "A Scope value together with an integer argument" $
  T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "integer">: gremlin "IntegerArgument"]

scopeStringArgs :: TypeDefinition
scopeStringArgs = define "ScopeStringArgs" $
  doc "A Scope value together with string arguments" $
  T.record [
  "scope">: gremlin "TraversalScopeArgument",
  "strings">: T.list $ gremlin "StringNullableArgument"]

selectArgs :: TypeDefinition
selectArgs = define "SelectArgs" $
  doc "The arguments of a Gremlin select() step" $
  T.union [
  "column">: gremlin "TraversalColumnArgument",
  "popStrings">: gremlin "SelectByKeys",
  "popTraversal">: gremlin "PopAndTraversal",
  "strings">: nonemptyList $ gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

selectByKeys :: TypeDefinition
selectByKeys = define "SelectByKeys" $
  doc "The keys selected by a Gremlin select() step" $
  T.record [
  "pop">: gremlin "TraversalPopArgument",
  "keys">: nonemptyList $ gremlin "StringArgument"]

serviceArguments :: TypeDefinition
serviceArguments = define "ServiceArguments" $
  doc "The arguments of a Gremlin call() service invocation" $
  T.union [
  "map">: T.optional $ gremlin "GenericLiteralMapArgument",
  "traversal">: T.optional $ gremlin "NestedTraversal"]

serviceCall :: TypeDefinition
serviceCall = define "ServiceCall" $
  doc "A Gremlin call() service invocation" $
  T.record [
  "service">: gremlin "StringArgument",
  "arguments">: gremlin "ServiceArguments"]

shortestPathConstants :: TypeDefinition
shortestPathConstants = define "ShortestPathConstants" $
  doc "The named constants used by the Gremlin shortestPath() step" $
  T.enum [
  "target",
  "edges",
  "distance",
  "maxDistance",
  "includeEdges"]

splitArgs :: TypeDefinition
splitArgs = define "SplitArgs" $
  doc "The arguments of a Gremlin split() step" $
  T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "delimiter">: gremlin "StringNullableArgument"]

stringAndObject :: TypeDefinition
stringAndObject = define "StringAndObject" $
  doc "A string argument together with an object argument" $
  T.record [
  "key">: gremlin "StringArgument",
  "value">: gremlin "GenericLiteralArgument"]

stringAndOptionalObject :: TypeDefinition
stringAndOptionalObject = define "StringAndOptionalObject" $
  doc "A string argument together with an optional object argument" $
  T.record [
  "key">: gremlin "StringArgument",
  "value">: T.optional $ gremlin "GenericLiteralArgument"]

stringArgument :: TypeDefinition
stringArgument = defArgumentDoc "StringArgument" "A Gremlin string argument: a literal value or a bound variable" T.string

stringArgumentOrNestedTraversal :: TypeDefinition
stringArgumentOrNestedTraversal = define "StringArgumentOrNestedTraversal" $
  doc "A string argument or a nested traversal" $
  T.union [
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

stringKeyAndObject :: TypeDefinition
stringKeyAndObject = define "StringKeyAndObject" $
  doc "A string key together with an object value" $
  T.record [
  "key">: gremlin "StringNullableArgument",
  "object">: gremlin "GenericLiteralArgument"]

stringKeyAndPredicate :: TypeDefinition
stringKeyAndPredicate = define "StringKeyAndPredicate" $
  doc "A string key together with a traversal predicate" $
  T.record [
  "key">: gremlin "StringNullableArgument",
  "predicate">: gremlin "TraversalPredicate"]

stringNullableArgument :: TypeDefinition
stringNullableArgument = defArgumentDoc "StringNullableArgument" "A Gremlin optional string argument: a literal value, a bound variable, or null" $
  T.optional T.string

stringRange :: TypeDefinition
stringRange = define "StringRange" $
  doc "A Gremlin string range expression" $
  T.record [
  "left">: T.string,
  "right">: T.string]

structureVertex :: TypeDefinition
structureVertex = define "StructureVertex" $
  doc "A Gremlin structure-level vertex reference" $
  T.record [
  "new">: T.boolean,
  "id">: gremlin "GenericLiteralArgument",
  "label">: gremlin "StringArgument"]

structureVertexArgument :: TypeDefinition
structureVertexArgument = defArgumentDoc "StructureVertexArgument" "A Gremlin structure-vertex argument: a literal value or a bound variable" $
  gremlin "StructureVertex"

substringArgs :: TypeDefinition
substringArgs = define "SubstringArgs" $
  doc "The arguments of a Gremlin substring() step" $
  T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "start">: gremlin "IntegerArgument",
  "end">: T.optional $ gremlin "IntegerArgument"]

tailArgs :: TypeDefinition
tailArgs = define "TailArgs" $
  doc "The arguments of a Gremlin tail() step" $
  T.record [
  "scope">: T.optional $ gremlin "TraversalScopeArgument",
  "integer">: T.optional $ gremlin "IntegerArgument"]

terminatedTraversal :: TypeDefinition
terminatedTraversal = define "TerminatedTraversal" $
  doc "A Gremlin traversal terminated by a terminal method" $
  T.record [
  "root">: gremlin "RootTraversal",
  "terminal">: gremlin "TraversalTerminalMethod"]

toArgs :: TypeDefinition
toArgs = define "ToArgs" $
  doc "The arguments of a Gremlin to() modulator" $
  T.union [
  "direction">: gremlin "DirectionAndVarargs",
  "string">: gremlin "StringArgument",
  "vertex">: gremlin "StructureVertexArgument",
  "traversal">: gremlin "NestedTraversal"]

transactionPart :: TypeDefinition
transactionPart = define "TransactionPart" $
  doc "A single part of a Gremlin transaction statement" $
  T.enum [
  "begin",
  "commit",
  "rollback"]

traversalBiFunctionArgument :: TypeDefinition
traversalBiFunctionArgument = defArgumentDoc "TraversalBiFunctionArgument" "A Gremlin BiFunction argument: a named function or a bound variable" $
  gremlin "TraversalOperator"

traversalCardinality :: TypeDefinition
traversalCardinality = define "TraversalCardinality" $
  doc "A Gremlin property cardinality: single, list, or set" $
  T.union [
  "single">: gremlin "GenericLiteral",
  "set">: gremlin "GenericLiteral",
  "list">: gremlin "GenericLiteral"]

traversalCardinalityArgument :: TypeDefinition
traversalCardinalityArgument = defArgumentDoc "TraversalCardinalityArgument" "A Gremlin cardinality argument: a literal value or a bound variable" $
  gremlin "TraversalCardinality"

traversalColumn :: TypeDefinition
traversalColumn = define "TraversalColumn" $
  doc "A Gremlin traversal column selector: keys or values" $
  T.enum [
  "keys",
  "values"]

traversalColumnArgument :: TypeDefinition
traversalColumnArgument = defArgumentDoc "TraversalColumnArgument" "A Gremlin column argument: a literal value or a bound variable" $
  gremlin "TraversalColumn"

traversalComparatorArgument :: TypeDefinition
traversalComparatorArgument = defArgumentDoc "TraversalComparatorArgument" "A Gremlin comparator argument: a named comparator or a bound variable" $
  gremlin "TraversalOrder"

traversalDirection :: TypeDefinition
traversalDirection = define "TraversalDirection" $
  doc "A Gremlin edge direction: in, out, or both" $
  T.enum [
  "in",
  "out",
  "both",
  "from",
  "to"]

traversalDirectionArgument :: TypeDefinition
traversalDirectionArgument = defArgumentDoc "TraversalDirectionArgument" "A Gremlin direction argument: a literal value or a bound variable" $
  gremlin "TraversalDirection"

traversalDT :: TypeDefinition
traversalDT = define "TraversalDT" $
  doc "A Gremlin date/time unit" $
  T.enum [
  "second",
  "minute",
  "hour",
  "day"]

traversalDTArgument :: TypeDefinition
traversalDTArgument = defArgumentDoc "TraversalDTArgument" "A Gremlin date/time-unit argument: a literal value or a bound variable" $
  gremlin "TraversalDT"

traversalFunction :: TypeDefinition
traversalFunction = define "TraversalFunction" $
  doc "A Gremlin named function reference" $
  T.union [
  "token">: gremlin "TraversalToken",
  "column">: gremlin "TraversalColumn"]

traversalFunctionArgument :: TypeDefinition
traversalFunctionArgument = defArgumentDoc "TraversalFunctionArgument" "A Gremlin function argument: a named function or a bound variable" $
  gremlin "TraversalFunction"

traversalGType :: TypeDefinition
traversalGType = define "TraversalGType" $
  doc "A Gremlin graph type constant" $
  T.enum [
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
traversalGTypeArgument = defArgumentDoc "TraversalGTypeArgument" "A Gremlin graph-type argument: a literal value or a bound variable" $
  gremlin "TraversalGType"

traversalMerge :: TypeDefinition
traversalMerge = define "TraversalMerge" $
  doc "A Gremlin merge-option selector" $
  T.enum [
  "onCreate",
  "onMatch",
  "outV",
  "inV"]

traversalMergeArgument :: TypeDefinition
traversalMergeArgument = defArgumentDoc "TraversalMergeArgument" "A Gremlin merge-option argument: a literal value or a bound variable" $
  gremlin "TraversalMerge"

traversalMethod :: TypeDefinition
traversalMethod = define "TraversalMethod" $
  doc "A single method call within a Gremlin traversal" $
  T.union [
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
traversalOperator = define "TraversalOperator" $
  doc "A Gremlin traversal operator" $
  T.enum [
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
traversalOrder = define "TraversalOrder" $
  doc "A Gremlin sort order: ascending, descending, or shuffle" $
  T.enum [
  "asc",
  "desc",
  "shuffle"]

traversalOrderArgument :: TypeDefinition
traversalOrderArgument = defArgumentDoc "TraversalOrderArgument" "A Gremlin order argument: a literal value or a bound variable" $
  gremlin "TraversalOrder"

traversalPick :: TypeDefinition
traversalPick = define "TraversalPick" $
  doc "A Gremlin Pick selector: any or none" $
  T.enum [
  "any",
  "none",
  "unproductive"]

traversalPop :: TypeDefinition
traversalPop = define "TraversalPop" $
  doc "A Gremlin Pop selector: first, last, all, or mixed" $
  T.enum [
  "first",
  "last",
  "all",
  "mixed"]

traversalPopArgument :: TypeDefinition
traversalPopArgument = defArgumentDoc "TraversalPopArgument" "A Gremlin Pop argument: a literal value or a bound variable" $
  gremlin "TraversalPop"

traversalPredicate :: TypeDefinition
traversalPredicate = define "TraversalPredicate" $
  doc "A Gremlin traversal predicate" $
  T.union [
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
traversalSackMethodArgument = defArgumentDoc "TraversalSackMethodArgument" "A Gremlin sack-method argument: a named function or a bound variable" T.unit

traversalScope :: TypeDefinition
traversalScope = define "TraversalScope" $
  doc "A Gremlin traversal scope: local or global" $
  T.enum [
  "local",
  "global"]

traversalScopeArgument :: TypeDefinition
traversalScopeArgument = defArgumentDoc "TraversalScopeArgument" "A Gremlin scope argument: a literal value or a bound variable" $
  gremlin "TraversalScope"

traversalSource :: TypeDefinition
traversalSource = define "TraversalSource" $
  doc "A Gremlin traversal source: g, or a variable bound to one" $
  T.wrap $ T.list $ gremlin "TraversalSourceSelfMethod"

traversalSourceQuery :: TypeDefinition
traversalSourceQuery = define "TraversalSourceQuery" $
  doc "A query built from a Gremlin traversal source" $
  T.record [
  "source">: gremlin "TraversalSource",
  "transactionPart">: T.optional $ gremlin "TransactionPart"]

traversalSourceSelfMethod :: TypeDefinition
traversalSourceSelfMethod = define "TraversalSourceSelfMethod" $
  doc "A traversal-source configuration method that returns the same source" $
  T.union [
  "withBulk">: T.boolean,
  "withPath">: T.unit,
  "withSack">: gremlin "WithSackArgs",
  "withSideEffect">: gremlin "StringAndObject",
  "withStrategies">: nonemptyList $ gremlin "TraversalStrategy",
  "withoutStrategies">: nonemptyList $ gremlin "Identifier",
  "with">: gremlin "StringAndOptionalObject"]

traversalSourceSpawnMethod :: TypeDefinition
traversalSourceSpawnMethod = define "TraversalSourceSpawnMethod" $
  doc "A traversal-source method that spawns a new traversal" $
  T.union [
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
traversalStrategy = define "TraversalStrategy" $
  doc "A Gremlin traversal strategy reference" $
  T.record [
  "new">: T.boolean,
  "class">: gremlin "Identifier",
  "configurations">: T.list $ gremlin "Configuration"]

traversalTerminalMethod :: TypeDefinition
traversalTerminalMethod = define "TraversalTerminalMethod" $
  doc "A terminal method that ends a Gremlin traversal, such as next() or toList()" $
  T.union [
  "explain">: T.unit,
  "iterate">: T.unit,
  "hasNext">: T.unit,
  "tryNext">: T.unit,
  "next">: T.optional $ gremlin "IntegerLiteral",
  "toList">: T.unit,
  "toSet">: T.unit,
  "toBulkSet">: T.unit]

traversalToken :: TypeDefinition
traversalToken = define "TraversalToken" $
  doc "A Gremlin element token: id, label, key, or value" $
  T.enum [
  "id",
  "label",
  "key",
  "value"]

traversalTokenArgument :: TypeDefinition
traversalTokenArgument = defArgumentDoc "TraversalTokenArgument" "A Gremlin token argument: a literal value or a bound variable" $
  gremlin "TraversalToken"

-- Literal types

twoTraversalPredicates :: TypeDefinition
twoTraversalPredicates = define "TwoTraversalPredicates" $
  doc "A pair of traversal predicates, as used in a between-style filter" $
  T.record [
  "left">: gremlin "TraversalPredicate",
  "right">: gremlin "TraversalPredicate"]

typeOfArg :: TypeDefinition
typeOfArg = define "TypeOfArg" $
  doc "The argument of a Gremlin type-checking predicate" $
  T.union [
  "gType">: gremlin "TraversalGType",
  "string">: gremlin "StringArgument"]

valueMapArgs :: TypeDefinition
valueMapArgs = define "ValueMapArgs" $
  doc "The arguments of a Gremlin valueMap() step" $
  T.union [
  "string">: nonemptyList $ gremlin "StringNullableArgument",
  "boolean">: gremlin "ValueMapBooleanArgs"]

valueMapBooleanArgs :: TypeDefinition
valueMapBooleanArgs = define "ValueMapBooleanArgs" $
  doc "The boolean-flag arguments of a Gremlin valueMap() step" $
  T.record [
  "value">: gremlin "BooleanArgument",
  "keys">: T.optional $ nonemptyList $ gremlin "StringNullableArgument"]

whereArgs :: TypeDefinition
whereArgs = define "WhereArgs" $
  doc "The arguments of a Gremlin where() step" $
  T.union [
  "predicate">: gremlin "WhereWithPredicateArgs",
  "string">: gremlin "StringArgument",
  "traversal">: gremlin "NestedTraversal"]

whereWithPredicateArgs :: TypeDefinition
whereWithPredicateArgs = define "WhereWithPredicateArgs" $
  doc "The arguments of a Gremlin where() step qualified by a predicate" $
  T.record [
  "leftArg">: T.optional $ gremlin "StringArgument",
  "predicate">: gremlin "TraversalPredicate"]

withArgs :: TypeDefinition
withArgs = define "WithArgs" $
  doc "The arguments of a Gremlin with() modulator" $
  T.record [
  "keys">: gremlin "WithArgsKeys",
  "values">: T.optional $ gremlin "WithArgsValues"]

withArgsKeys :: TypeDefinition
withArgsKeys = define "WithArgsKeys" $
  doc "The named keys accepted by a Gremlin with() modulator" $
  T.union [
  "withOption">: gremlin "WithOptionKeys",
  "string">: gremlin "StringArgument"]

withArgsValues :: TypeDefinition
withArgsValues = define "WithArgsValues" $
  doc "The named values accepted by a Gremlin with() modulator" $
  T.union [
  "withOptions">: gremlin "WithOptionsValues",
  "io">: gremlin "IoOptionsValues",
  "object">: gremlin "GenericLiteralArgument"]

withOptionKeys :: TypeDefinition
withOptionKeys = define "WithOptionKeys" $
  doc "The named option keys accepted by a Gremlin with() modulator" $
  T.union [
  "shortestPath">: gremlin "ShortestPathConstants",
  "connectedComponent">: gremlin "ConnectedComponentConstants",
  "pageRank">: gremlin "PageRankConstants",
  "peerPressure">: gremlin "PeerPressureConstants",
  "io">: gremlin "IoOptionsKeys",
  "withOptionsTokens">: T.unit,
  "withOptionsIndexer">: T.unit]

withOptionsValues :: TypeDefinition
withOptionsValues = define "WithOptionsValues" $
  doc "The named option values accepted by a Gremlin with() modulator" $
  T.enum [
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
withSackArgs = define "WithSackArgs" $
  doc "The arguments of a Gremlin withSack() step" $
  T.record [
  "initialValue">: gremlin "GenericLiteralArgument",
  "biFunction">: T.optional $ gremlin "TraversalBiFunctionArgument"]
