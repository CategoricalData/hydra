-- Note: this is an automatically generated file. Do not edit.
-- | A Gremlin model, based on the Apache TinkerPop Gremlin ANTLR grammar (version 3.8.1). This is a streamlined model: it preserves the semantics of the grammar (steps, predicates, arguments, literals) while collapsing pure parser-production artifacts (single-use 'XAndY'/'XOrY' glue types and left-recursion 'rest' continuations) so that the representation maps cleanly onto TinkerPop's native traversal model.

module Hydra.Tinkerpop.Gremlin where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I
data AsArgs =
  AsArgs {
    asArgsFirst :: StringArgument,
    asArgsRest :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)
_AsArgs = Core.Name "hydra.tinkerpop.gremlin.AsArgs"
_AsArgs_first = Core.Name "first"
_AsArgs_rest = Core.Name "rest"
data BarrierArgs =
  BarrierArgsConsumer TraversalSackMethodArgument |
  BarrierArgsInt IntegerArgument
  deriving (Eq, Ord, Read, Show)
_BarrierArgs = Core.Name "hydra.tinkerpop.gremlin.BarrierArgs"
_BarrierArgs_consumer = Core.Name "consumer"
_BarrierArgs_int = Core.Name "int"
data BooleanArgument =
  BooleanArgumentValue Bool |
  BooleanArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_BooleanArgument = Core.Name "hydra.tinkerpop.gremlin.BooleanArgument"
_BooleanArgument_value = Core.Name "value"
_BooleanArgument_variable = Core.Name "variable"
data ByArgs =
  ByArgsOrder TraversalOrderArgument |
  ByArgsToken TraversalTokenArgument |
  ByArgsOther ByOtherArgs
  deriving (Eq, Ord, Read, Show)
_ByArgs = Core.Name "hydra.tinkerpop.gremlin.ByArgs"
_ByArgs_order = Core.Name "order"
_ByArgs_token = Core.Name "token"
_ByArgs_other = Core.Name "other"
data ByOtherArgs =
  ByOtherArgsComparator (Maybe TraversalComparatorArgument) |
  ByOtherArgsFunction TraversalFunctionArgument |
  ByOtherArgsString StringArgument |
  ByOtherArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_ByOtherArgs = Core.Name "hydra.tinkerpop.gremlin.ByOtherArgs"
_ByOtherArgs_comparator = Core.Name "comparator"
_ByOtherArgs_function = Core.Name "function"
_ByOtherArgs_string = Core.Name "string"
_ByOtherArgs_traversal = Core.Name "traversal"
data CardinalityAndMap =
  CardinalityAndMap {
    cardinalityAndMapCardinality :: TraversalCardinalityArgument,
    cardinalityAndMapObject :: GenericLiteralMapNullableArgument}
  deriving (Eq, Ord, Read, Show)
_CardinalityAndMap = Core.Name "hydra.tinkerpop.gremlin.CardinalityAndMap"
_CardinalityAndMap_cardinality = Core.Name "cardinality"
_CardinalityAndMap_object = Core.Name "object"
data CardinalityAndObjects =
  CardinalityAndObjects {
    cardinalityAndObjectsCardinality :: TraversalCardinalityArgument,
    cardinalityAndObjectsObjects :: [GenericLiteralArgument]}
  deriving (Eq, Ord, Read, Show)
_CardinalityAndObjects = Core.Name "hydra.tinkerpop.gremlin.CardinalityAndObjects"
_CardinalityAndObjects_cardinality = Core.Name "cardinality"
_CardinalityAndObjects_objects = Core.Name "objects"
newtype ChainedTraversal =
  ChainedTraversal {
    unChainedTraversal :: [TraversalMethod]}
  deriving (Eq, Ord, Read, Show)
_ChainedTraversal = Core.Name "hydra.tinkerpop.gremlin.ChainedTraversal"
data ChooseArgs =
  ChooseArgsFunction TraversalFunctionArgument |
  ChooseArgsPredicateTraversal PredicateOrTraversalChoice |
  ChooseArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_ChooseArgs = Core.Name "hydra.tinkerpop.gremlin.ChooseArgs"
_ChooseArgs_function = Core.Name "function"
_ChooseArgs_predicateTraversal = Core.Name "predicateTraversal"
_ChooseArgs_traversal = Core.Name "traversal"
data ConcatArgs =
  ConcatArgsTraversal [NestedTraversal] |
  ConcatArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)
_ConcatArgs = Core.Name "hydra.tinkerpop.gremlin.ConcatArgs"
_ConcatArgs_traversal = Core.Name "traversal"
_ConcatArgs_string = Core.Name "string"
data Configuration =
  Configuration {
    configurationKey :: KeywordOrIdentifier,
    configurationValue :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)
_Configuration = Core.Name "hydra.tinkerpop.gremlin.Configuration"
_Configuration_key = Core.Name "key"
_Configuration_value = Core.Name "value"
data ConnectedComponentConstants =
  ConnectedComponentConstantsComponent |
  ConnectedComponentConstantsEdges |
  ConnectedComponentConstantsPropertyName
  deriving (Eq, Ord, Read, Show)
_ConnectedComponentConstants = Core.Name "hydra.tinkerpop.gremlin.ConnectedComponentConstants"
_ConnectedComponentConstants_component = Core.Name "component"
_ConnectedComponentConstants_edges = Core.Name "edges"
_ConnectedComponentConstants_propertyName = Core.Name "propertyName"
data DateAddArgs =
  DateAddArgs {
    dateAddArgsUnit :: TraversalDTArgument,
    dateAddArgsDuration :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)
_DateAddArgs = Core.Name "hydra.tinkerpop.gremlin.DateAddArgs"
_DateAddArgs_unit = Core.Name "unit"
_DateAddArgs_duration = Core.Name "duration"
data DateArgument =
  DateArgumentValue DateLiteral |
  DateArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_DateArgument = Core.Name "hydra.tinkerpop.gremlin.DateArgument"
_DateArgument_value = Core.Name "value"
_DateArgument_variable = Core.Name "variable"
data DateDiffArgs =
  DateDiffArgsTraversal NestedTraversal |
  DateDiffArgsDate DateArgument
  deriving (Eq, Ord, Read, Show)
_DateDiffArgs = Core.Name "hydra.tinkerpop.gremlin.DateDiffArgs"
_DateDiffArgs_traversal = Core.Name "traversal"
_DateDiffArgs_date = Core.Name "date"
newtype DateLiteral =
  DateLiteral {
    unDateLiteral :: (Maybe StringArgument)}
  deriving (Eq, Ord, Read, Show)
_DateLiteral = Core.Name "hydra.tinkerpop.gremlin.DateLiteral"
data DedupArgs =
  DedupArgsScopeString ScopeStringArgs |
  DedupArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)
_DedupArgs = Core.Name "hydra.tinkerpop.gremlin.DedupArgs"
_DedupArgs_scopeString = Core.Name "scopeString"
_DedupArgs_string = Core.Name "string"
data DirectionAndVarargs =
  DirectionAndVarargs {
    directionAndVarargsDirection :: TraversalDirectionArgument,
    directionAndVarargsVarargs :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)
_DirectionAndVarargs = Core.Name "hydra.tinkerpop.gremlin.DirectionAndVarargs"
_DirectionAndVarargs_direction = Core.Name "direction"
_DirectionAndVarargs_varargs = Core.Name "varargs"
data FloatArgument =
  FloatArgumentValue FloatLiteral |
  FloatArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_FloatArgument = Core.Name "hydra.tinkerpop.gremlin.FloatArgument"
_FloatArgument_value = Core.Name "value"
_FloatArgument_variable = Core.Name "variable"
data FloatLiteral =
  FloatLiteralFloat Float |
  FloatLiteralDouble Double |
  FloatLiteralBig Sci.Scientific
  deriving (Eq, Ord, Read, Show)
_FloatLiteral = Core.Name "hydra.tinkerpop.gremlin.FloatLiteral"
_FloatLiteral_float = Core.Name "float"
_FloatLiteral_double = Core.Name "double"
_FloatLiteral_big = Core.Name "big"
data FoldArgs =
  FoldArgs {
    foldArgsSeed :: GenericLiteralArgument,
    foldArgsBiFunction :: TraversalBiFunctionArgument}
  deriving (Eq, Ord, Read, Show)
_FoldArgs = Core.Name "hydra.tinkerpop.gremlin.FoldArgs"
_FoldArgs_seed = Core.Name "seed"
_FoldArgs_biFunction = Core.Name "biFunction"
data FromArgs =
  FromArgsString StringArgument |
  FromArgsVertex StructureVertexArgument |
  FromArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_FromArgs = Core.Name "hydra.tinkerpop.gremlin.FromArgs"
_FromArgs_string = Core.Name "string"
_FromArgs_vertex = Core.Name "vertex"
_FromArgs_traversal = Core.Name "traversal"
data GenericLiteral =
  GenericLiteralNumeric NumericLiteral |
  GenericLiteralBoolean Bool |
  GenericLiteralString String |
  GenericLiteralDate DateLiteral |
  GenericLiteralNull |
  GenericLiteralNan |
  GenericLiteralInf |
  GenericLiteralTraversalToken TraversalToken |
  GenericLiteralTraversalCardinality TraversalCardinality |
  GenericLiteralTraversalDirection TraversalDirection |
  GenericLiteralTraversalMerge TraversalMerge |
  GenericLiteralTraversalPick TraversalPick |
  GenericLiteralTraversalDT TraversalDT |
  GenericLiteralTraversalGType TraversalGType |
  GenericLiteralStructureVertex StructureVertex |
  GenericLiteralGenericLiteralSet GenericLiteralSet |
  GenericLiteralGenericLiteralCollection GenericLiteralCollection |
  GenericLiteralGenericLiteralRange GenericLiteralRange |
  GenericLiteralNestedTraversal NestedTraversal |
  GenericLiteralTerminatedTraversal TerminatedTraversal |
  GenericLiteralGenericLiteralMap GenericLiteralMap
  deriving (Eq, Ord, Read, Show)
_GenericLiteral = Core.Name "hydra.tinkerpop.gremlin.GenericLiteral"
_GenericLiteral_numeric = Core.Name "numeric"
_GenericLiteral_boolean = Core.Name "boolean"
_GenericLiteral_string = Core.Name "string"
_GenericLiteral_date = Core.Name "date"
_GenericLiteral_null = Core.Name "null"
_GenericLiteral_nan = Core.Name "nan"
_GenericLiteral_inf = Core.Name "inf"
_GenericLiteral_traversalToken = Core.Name "traversalToken"
_GenericLiteral_traversalCardinality = Core.Name "traversalCardinality"
_GenericLiteral_traversalDirection = Core.Name "traversalDirection"
_GenericLiteral_traversalMerge = Core.Name "traversalMerge"
_GenericLiteral_traversalPick = Core.Name "traversalPick"
_GenericLiteral_traversalDT = Core.Name "traversalDT"
_GenericLiteral_traversalGType = Core.Name "traversalGType"
_GenericLiteral_structureVertex = Core.Name "structureVertex"
_GenericLiteral_genericLiteralSet = Core.Name "genericLiteralSet"
_GenericLiteral_genericLiteralCollection = Core.Name "genericLiteralCollection"
_GenericLiteral_genericLiteralRange = Core.Name "genericLiteralRange"
_GenericLiteral_nestedTraversal = Core.Name "nestedTraversal"
_GenericLiteral_terminatedTraversal = Core.Name "terminatedTraversal"
_GenericLiteral_genericLiteralMap = Core.Name "genericLiteralMap"
data GenericLiteralArgument =
  GenericLiteralArgumentValue GenericLiteral |
  GenericLiteralArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_GenericLiteralArgument = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralArgument"
_GenericLiteralArgument_value = Core.Name "value"
_GenericLiteralArgument_variable = Core.Name "variable"
newtype GenericLiteralCollection =
  GenericLiteralCollection {
    unGenericLiteralCollection :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)
_GenericLiteralCollection = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralCollection"
newtype GenericLiteralList =
  GenericLiteralList {
    unGenericLiteralList :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)
_GenericLiteralList = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralList"
data GenericLiteralListArgument =
  GenericLiteralListArgumentValue GenericLiteralList |
  GenericLiteralListArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_GenericLiteralListArgument = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralListArgument"
_GenericLiteralListArgument_value = Core.Name "value"
_GenericLiteralListArgument_variable = Core.Name "variable"
newtype GenericLiteralMap =
  GenericLiteralMap {
    unGenericLiteralMap :: [MapEntry]}
  deriving (Eq, Ord, Read, Show)
_GenericLiteralMap = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMap"
data GenericLiteralMapArgument =
  GenericLiteralMapArgumentValue GenericLiteralMap |
  GenericLiteralMapArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_GenericLiteralMapArgument = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapArgument"
_GenericLiteralMapArgument_value = Core.Name "value"
_GenericLiteralMapArgument_variable = Core.Name "variable"
data GenericLiteralMapNullableArgument =
  GenericLiteralMapNullableArgumentValue (Maybe GenericLiteralMap) |
  GenericLiteralMapNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_GenericLiteralMapNullableArgument = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralMapNullableArgument"
_GenericLiteralMapNullableArgument_value = Core.Name "value"
_GenericLiteralMapNullableArgument_variable = Core.Name "variable"
data GenericLiteralRange =
  GenericLiteralRangeInteger IntegerRange |
  GenericLiteralRangeString StringRange
  deriving (Eq, Ord, Read, Show)
_GenericLiteralRange = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralRange"
_GenericLiteralRange_integer = Core.Name "integer"
_GenericLiteralRange_string = Core.Name "string"
newtype GenericLiteralSet =
  GenericLiteralSet {
    unGenericLiteralSet :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)
_GenericLiteralSet = Core.Name "hydra.tinkerpop.gremlin.GenericLiteralSet"
data HasArgs =
  HasArgsString HasArgsWithKey |
  HasArgsTraversalToken HasArgsWithToken
  deriving (Eq, Ord, Read, Show)
_HasArgs = Core.Name "hydra.tinkerpop.gremlin.HasArgs"
_HasArgs_string = Core.Name "string"
_HasArgs_traversalToken = Core.Name "traversalToken"
data HasArgsWithKey =
  HasArgsWithKey {
    hasArgsWithKeyKey :: StringNullableArgument,
    hasArgsWithKeyValue :: (Maybe HasValueClause)}
  deriving (Eq, Ord, Read, Show)
_HasArgsWithKey = Core.Name "hydra.tinkerpop.gremlin.HasArgsWithKey"
_HasArgsWithKey_key = Core.Name "key"
_HasArgsWithKey_value = Core.Name "value"
data HasArgsWithToken =
  HasArgsWithToken {
    hasArgsWithTokenToken :: TraversalTokenArgument,
    hasArgsWithTokenValue :: HasValueClause}
  deriving (Eq, Ord, Read, Show)
_HasArgsWithToken = Core.Name "hydra.tinkerpop.gremlin.HasArgsWithToken"
_HasArgsWithToken_token = Core.Name "token"
_HasArgsWithToken_value = Core.Name "value"
data HasValueClause =
  HasValueClauseObject GenericLiteralArgument |
  HasValueClausePredicate TraversalPredicate |
  HasValueClauseTraversal NestedTraversal |
  HasValueClauseKeyObject StringKeyAndObject |
  HasValueClauseKeyPredicate StringKeyAndPredicate
  deriving (Eq, Ord, Read, Show)
_HasValueClause = Core.Name "hydra.tinkerpop.gremlin.HasValueClause"
_HasValueClause_object = Core.Name "object"
_HasValueClause_predicate = Core.Name "predicate"
_HasValueClause_traversal = Core.Name "traversal"
_HasValueClause_keyObject = Core.Name "keyObject"
_HasValueClause_keyPredicate = Core.Name "keyPredicate"
newtype Identifier =
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)
_Identifier = Core.Name "hydra.tinkerpop.gremlin.Identifier"
data IntegerArgument =
  IntegerArgumentValue IntegerLiteral |
  IntegerArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_IntegerArgument = Core.Name "hydra.tinkerpop.gremlin.IntegerArgument"
_IntegerArgument_value = Core.Name "value"
_IntegerArgument_variable = Core.Name "variable"
data IntegerLiteral =
  IntegerLiteralByte I.Int8 |
  IntegerLiteralShort I.Int16 |
  IntegerLiteralInt Int |
  IntegerLiteralLong I.Int64 |
  IntegerLiteralBig Integer
  deriving (Eq, Ord, Read, Show)
_IntegerLiteral = Core.Name "hydra.tinkerpop.gremlin.IntegerLiteral"
_IntegerLiteral_byte = Core.Name "byte"
_IntegerLiteral_short = Core.Name "short"
_IntegerLiteral_int = Core.Name "int"
_IntegerLiteral_long = Core.Name "long"
_IntegerLiteral_big = Core.Name "big"
data IntegerRange =
  IntegerRange {
    integerRangeLeft :: IntegerLiteral,
    integerRangeRight :: IntegerLiteral}
  deriving (Eq, Ord, Read, Show)
_IntegerRange = Core.Name "hydra.tinkerpop.gremlin.IntegerRange"
_IntegerRange_left = Core.Name "left"
_IntegerRange_right = Core.Name "right"
data IoOptionsKeys =
  IoOptionsKeysReader |
  IoOptionsKeysWriter
  deriving (Eq, Ord, Read, Show)
_IoOptionsKeys = Core.Name "hydra.tinkerpop.gremlin.IoOptionsKeys"
_IoOptionsKeys_reader = Core.Name "reader"
_IoOptionsKeys_writer = Core.Name "writer"
data IoOptionsValues =
  IoOptionsValuesGryo |
  IoOptionsValuesGraphson |
  IoOptionsValuesGraphml
  deriving (Eq, Ord, Read, Show)
_IoOptionsValues = Core.Name "hydra.tinkerpop.gremlin.IoOptionsValues"
_IoOptionsValues_gryo = Core.Name "gryo"
_IoOptionsValues_graphson = Core.Name "graphson"
_IoOptionsValues_graphml = Core.Name "graphml"
data Keyword =
  KeywordEdges |
  KeywordKeys |
  KeywordNew |
  KeywordValues
  deriving (Eq, Ord, Read, Show)
_Keyword = Core.Name "hydra.tinkerpop.gremlin.Keyword"
_Keyword_edges = Core.Name "edges"
_Keyword_keys = Core.Name "keys"
_Keyword_new = Core.Name "new"
_Keyword_values = Core.Name "values"
data KeywordOrIdentifier =
  KeywordOrIdentifierKeyword Keyword |
  KeywordOrIdentifierIdentifier Identifier
  deriving (Eq, Ord, Read, Show)
_KeywordOrIdentifier = Core.Name "hydra.tinkerpop.gremlin.KeywordOrIdentifier"
_KeywordOrIdentifier_keyword = Core.Name "keyword"
_KeywordOrIdentifier_identifier = Core.Name "identifier"
data MapEntry =
  MapEntryKey MapKey |
  MapEntryValue GenericLiteral
  deriving (Eq, Ord, Read, Show)
_MapEntry = Core.Name "hydra.tinkerpop.gremlin.MapEntry"
_MapEntry_key = Core.Name "key"
_MapEntry_value = Core.Name "value"
data MapKey =
  MapKeyString String |
  MapKeyNumeric NumericLiteral |
  MapKeyTraversalToken TraversalToken |
  MapKeyTraversalDirection TraversalDirection |
  MapKeySet GenericLiteralSet |
  MapKeyCollection GenericLiteralCollection |
  MapKeyMap GenericLiteralMap |
  MapKeyKeyword Keyword |
  MapKeyIdentifier Identifier
  deriving (Eq, Ord, Read, Show)
_MapKey = Core.Name "hydra.tinkerpop.gremlin.MapKey"
_MapKey_string = Core.Name "string"
_MapKey_numeric = Core.Name "numeric"
_MapKey_traversalToken = Core.Name "traversalToken"
_MapKey_traversalDirection = Core.Name "traversalDirection"
_MapKey_set = Core.Name "set"
_MapKey_collection = Core.Name "collection"
_MapKey_map = Core.Name "map"
_MapKey_keyword = Core.Name "keyword"
_MapKey_identifier = Core.Name "identifier"
data MergeArgs =
  MergeArgsMap GenericLiteralMapNullableArgument |
  MergeArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_MergeArgs = Core.Name "hydra.tinkerpop.gremlin.MergeArgs"
_MergeArgs_map = Core.Name "map"
_MergeArgs_traversal = Core.Name "traversal"
data MergeMapOption =
  MergeMapOption {
    mergeMapOptionMerge :: TraversalMergeArgument,
    mergeMapOptionMap :: GenericLiteralMapNullableArgument,
    mergeMapOptionCardinality :: (Maybe TraversalCardinality)}
  deriving (Eq, Ord, Read, Show)
_MergeMapOption = Core.Name "hydra.tinkerpop.gremlin.MergeMapOption"
_MergeMapOption_merge = Core.Name "merge"
_MergeMapOption_map = Core.Name "map"
_MergeMapOption_cardinality = Core.Name "cardinality"
data MergeTraversalOption =
  MergeTraversalOption {
    mergeTraversalOptionMerge :: TraversalMergeArgument,
    mergeTraversalOptionTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)
_MergeTraversalOption = Core.Name "hydra.tinkerpop.gremlin.MergeTraversalOption"
_MergeTraversalOption_merge = Core.Name "merge"
_MergeTraversalOption_traversal = Core.Name "traversal"
data NestedTraversal =
  NestedTraversalRoot RootTraversal |
  NestedTraversalChained ChainedTraversal |
  NestedTraversalAnonymous ChainedTraversal
  deriving (Eq, Ord, Read, Show)
_NestedTraversal = Core.Name "hydra.tinkerpop.gremlin.NestedTraversal"
_NestedTraversal_root = Core.Name "root"
_NestedTraversal_chained = Core.Name "chained"
_NestedTraversal_anonymous = Core.Name "anonymous"
data NumericArgument =
  NumericArgumentValue NumericLiteral |
  NumericArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_NumericArgument = Core.Name "hydra.tinkerpop.gremlin.NumericArgument"
_NumericArgument_value = Core.Name "value"
_NumericArgument_variable = Core.Name "variable"
data NumericLiteral =
  NumericLiteralInteger IntegerLiteral |
  NumericLiteralFloat FloatLiteral
  deriving (Eq, Ord, Read, Show)
_NumericLiteral = Core.Name "hydra.tinkerpop.gremlin.NumericLiteral"
_NumericLiteral_integer = Core.Name "integer"
_NumericLiteral_float = Core.Name "float"
data ObjectAndTraversal =
  ObjectAndTraversal {
    objectAndTraversalObject :: GenericLiteralArgument,
    objectAndTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)
_ObjectAndTraversal = Core.Name "hydra.tinkerpop.gremlin.ObjectAndTraversal"
_ObjectAndTraversal_object = Core.Name "object"
_ObjectAndTraversal_traversal = Core.Name "traversal"
data OptionArgs =
  OptionArgsPredicateTraversal PredicateAndTraversal |
  OptionArgsMergeMap MergeMapOption |
  OptionArgsMergeTraversal MergeTraversalOption |
  OptionArgsObjectTraversal ObjectAndTraversal |
  OptionArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_OptionArgs = Core.Name "hydra.tinkerpop.gremlin.OptionArgs"
_OptionArgs_predicateTraversal = Core.Name "predicateTraversal"
_OptionArgs_mergeMap = Core.Name "mergeMap"
_OptionArgs_mergeTraversal = Core.Name "mergeTraversal"
_OptionArgs_objectTraversal = Core.Name "objectTraversal"
_OptionArgs_traversal = Core.Name "traversal"
data PageRankConstants =
  PageRankConstantsEdges |
  PageRankConstantsTimes |
  PageRankConstantsPropertyName
  deriving (Eq, Ord, Read, Show)
_PageRankConstants = Core.Name "hydra.tinkerpop.gremlin.PageRankConstants"
_PageRankConstants_edges = Core.Name "edges"
_PageRankConstants_times = Core.Name "times"
_PageRankConstants_propertyName = Core.Name "propertyName"
data PeerPressureConstants =
  PeerPressureConstantsEdges |
  PeerPressureConstantsTimes |
  PeerPressureConstantsPropertyName
  deriving (Eq, Ord, Read, Show)
_PeerPressureConstants = Core.Name "hydra.tinkerpop.gremlin.PeerPressureConstants"
_PeerPressureConstants_edges = Core.Name "edges"
_PeerPressureConstants_times = Core.Name "times"
_PeerPressureConstants_propertyName = Core.Name "propertyName"
data PopAndTraversal =
  PopAndTraversal {
    popAndTraversalPop :: TraversalPopArgument,
    popAndTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)
_PopAndTraversal = Core.Name "hydra.tinkerpop.gremlin.PopAndTraversal"
_PopAndTraversal_pop = Core.Name "pop"
_PopAndTraversal_traversal = Core.Name "traversal"
data PredicateAndTraversal =
  PredicateAndTraversal {
    predicateAndTraversalPredicate :: TraversalPredicate,
    predicateAndTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)
_PredicateAndTraversal = Core.Name "hydra.tinkerpop.gremlin.PredicateAndTraversal"
_PredicateAndTraversal_predicate = Core.Name "predicate"
_PredicateAndTraversal_traversal = Core.Name "traversal"
data PredicateOrObject =
  PredicateOrObjectPredicate TraversalPredicate |
  PredicateOrObjectObject GenericLiteralArgument
  deriving (Eq, Ord, Read, Show)
_PredicateOrObject = Core.Name "hydra.tinkerpop.gremlin.PredicateOrObject"
_PredicateOrObject_predicate = Core.Name "predicate"
_PredicateOrObject_object = Core.Name "object"
data PredicateOrObjects =
  PredicateOrObjectsPredicate TraversalPredicate |
  PredicateOrObjectsObjects [GenericLiteralArgument]
  deriving (Eq, Ord, Read, Show)
_PredicateOrObjects = Core.Name "hydra.tinkerpop.gremlin.PredicateOrObjects"
_PredicateOrObjects_predicate = Core.Name "predicate"
_PredicateOrObjects_objects = Core.Name "objects"
data PredicateOrStrings =
  PredicateOrStringsPredicate TraversalPredicate |
  PredicateOrStringsStrings [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)
_PredicateOrStrings = Core.Name "hydra.tinkerpop.gremlin.PredicateOrStrings"
_PredicateOrStrings_predicate = Core.Name "predicate"
_PredicateOrStrings_strings = Core.Name "strings"
data PredicateOrTraversal =
  PredicateOrTraversalPredicate TraversalPredicate |
  PredicateOrTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_PredicateOrTraversal = Core.Name "hydra.tinkerpop.gremlin.PredicateOrTraversal"
_PredicateOrTraversal_predicate = Core.Name "predicate"
_PredicateOrTraversal_traversal = Core.Name "traversal"
data PredicateOrTraversalChoice =
  PredicateOrTraversalChoice {
    predicateOrTraversalChoicePredicate :: TraversalPredicate,
    predicateOrTraversalChoiceTrue :: NestedTraversal,
    predicateOrTraversalChoiceFalse :: (Maybe NestedTraversal)}
  deriving (Eq, Ord, Read, Show)
_PredicateOrTraversalChoice = Core.Name "hydra.tinkerpop.gremlin.PredicateOrTraversalChoice"
_PredicateOrTraversalChoice_predicate = Core.Name "predicate"
_PredicateOrTraversalChoice_true = Core.Name "true"
_PredicateOrTraversalChoice_false = Core.Name "false"
data PropertyArgs =
  PropertyArgsCardinalityObjects CardinalityAndObjects |
  PropertyArgsObjects [GenericLiteralArgument] |
  PropertyArgsObject GenericLiteralMapNullableArgument |
  PropertyArgsCardinalityObject CardinalityAndMap
  deriving (Eq, Ord, Read, Show)
_PropertyArgs = Core.Name "hydra.tinkerpop.gremlin.PropertyArgs"
_PropertyArgs_cardinalityObjects = Core.Name "cardinalityObjects"
_PropertyArgs_objects = Core.Name "objects"
_PropertyArgs_object = Core.Name "object"
_PropertyArgs_cardinalityObject = Core.Name "cardinalityObject"
data Query =
  QueryTraversalSource TraversalSourceQuery |
  QueryRootTraversal RootTraversalQuery |
  QueryToString |
  QueryEmpty
  deriving (Eq, Ord, Read, Show)
_Query = Core.Name "hydra.tinkerpop.gremlin.Query"
_Query_traversalSource = Core.Name "traversalSource"
_Query_rootTraversal = Core.Name "rootTraversal"
_Query_toString = Core.Name "toString"
_Query_empty = Core.Name "empty"
newtype QueryList =
  QueryList {
    unQueryList :: [Query]}
  deriving (Eq, Ord, Read, Show)
_QueryList = Core.Name "hydra.tinkerpop.gremlin.QueryList"
data RangeArgs =
  RangeArgs {
    rangeArgsScope :: (Maybe TraversalScopeArgument),
    rangeArgsMin :: IntegerArgument,
    rangeArgsMax :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)
_RangeArgs = Core.Name "hydra.tinkerpop.gremlin.RangeArgs"
_RangeArgs_scope = Core.Name "scope"
_RangeArgs_min = Core.Name "min"
_RangeArgs_max = Core.Name "max"
data RangeArgument =
  RangeArgument {
    rangeArgumentMin :: GenericLiteralArgument,
    rangeArgumentMax :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)
_RangeArgument = Core.Name "hydra.tinkerpop.gremlin.RangeArgument"
_RangeArgument_min = Core.Name "min"
_RangeArgument_max = Core.Name "max"
data RepeatArgs =
  RepeatArgs {
    repeatArgsString :: (Maybe StringArgument),
    repeatArgsTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)
_RepeatArgs = Core.Name "hydra.tinkerpop.gremlin.RepeatArgs"
_RepeatArgs_string = Core.Name "string"
_RepeatArgs_traversal = Core.Name "traversal"
data ReplaceArgs =
  ReplaceArgs {
    replaceArgsScope :: (Maybe TraversalScopeArgument),
    replaceArgsFrom :: StringNullableArgument,
    replaceArgsTo :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)
_ReplaceArgs = Core.Name "hydra.tinkerpop.gremlin.ReplaceArgs"
_ReplaceArgs_scope = Core.Name "scope"
_ReplaceArgs_from = Core.Name "from"
_ReplaceArgs_to = Core.Name "to"
data RootTraversal =
  RootTraversal {
    rootTraversalSource :: TraversalSource,
    rootTraversalSpawnMethod :: TraversalSourceSpawnMethod,
    rootTraversalChained :: [TraversalMethod]}
  deriving (Eq, Ord, Read, Show)
_RootTraversal = Core.Name "hydra.tinkerpop.gremlin.RootTraversal"
_RootTraversal_source = Core.Name "source"
_RootTraversal_spawnMethod = Core.Name "spawnMethod"
_RootTraversal_chained = Core.Name "chained"
data RootTraversalQuery =
  RootTraversalQuery {
    rootTraversalQueryRoot :: RootTraversal,
    rootTraversalQueryTerminalMethod :: (Maybe TraversalTerminalMethod)}
  deriving (Eq, Ord, Read, Show)
_RootTraversalQuery = Core.Name "hydra.tinkerpop.gremlin.RootTraversalQuery"
_RootTraversalQuery_root = Core.Name "root"
_RootTraversalQuery_terminalMethod = Core.Name "terminalMethod"
data SampleByScope =
  SampleByScope {
    sampleByScopeScope :: (Maybe TraversalScopeArgument),
    sampleByScopeInteger :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)
_SampleByScope = Core.Name "hydra.tinkerpop.gremlin.SampleByScope"
_SampleByScope_scope = Core.Name "scope"
_SampleByScope_integer = Core.Name "integer"
data ScopeAndInteger =
  ScopeAndInteger {
    scopeAndIntegerScope :: (Maybe TraversalScopeArgument),
    scopeAndIntegerInteger :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)
_ScopeAndInteger = Core.Name "hydra.tinkerpop.gremlin.ScopeAndInteger"
_ScopeAndInteger_scope = Core.Name "scope"
_ScopeAndInteger_integer = Core.Name "integer"
data ScopeStringArgs =
  ScopeStringArgs {
    scopeStringArgsScope :: TraversalScopeArgument,
    scopeStringArgsStrings :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)
_ScopeStringArgs = Core.Name "hydra.tinkerpop.gremlin.ScopeStringArgs"
_ScopeStringArgs_scope = Core.Name "scope"
_ScopeStringArgs_strings = Core.Name "strings"
data SelectArgs =
  SelectArgsColumn TraversalColumnArgument |
  SelectArgsPopStrings SelectByKeys |
  SelectArgsPopTraversal PopAndTraversal |
  SelectArgsStrings [StringArgument] |
  SelectArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_SelectArgs = Core.Name "hydra.tinkerpop.gremlin.SelectArgs"
_SelectArgs_column = Core.Name "column"
_SelectArgs_popStrings = Core.Name "popStrings"
_SelectArgs_popTraversal = Core.Name "popTraversal"
_SelectArgs_strings = Core.Name "strings"
_SelectArgs_traversal = Core.Name "traversal"
data SelectByKeys =
  SelectByKeys {
    selectByKeysPop :: TraversalPopArgument,
    selectByKeysKeys :: [StringArgument]}
  deriving (Eq, Ord, Read, Show)
_SelectByKeys = Core.Name "hydra.tinkerpop.gremlin.SelectByKeys"
_SelectByKeys_pop = Core.Name "pop"
_SelectByKeys_keys = Core.Name "keys"
data ServiceArguments =
  ServiceArgumentsMap (Maybe GenericLiteralMapArgument) |
  ServiceArgumentsTraversal (Maybe NestedTraversal)
  deriving (Eq, Ord, Read, Show)
_ServiceArguments = Core.Name "hydra.tinkerpop.gremlin.ServiceArguments"
_ServiceArguments_map = Core.Name "map"
_ServiceArguments_traversal = Core.Name "traversal"
data ServiceCall =
  ServiceCall {
    serviceCallService :: StringArgument,
    serviceCallArguments :: ServiceArguments}
  deriving (Eq, Ord, Read, Show)
_ServiceCall = Core.Name "hydra.tinkerpop.gremlin.ServiceCall"
_ServiceCall_service = Core.Name "service"
_ServiceCall_arguments = Core.Name "arguments"
data ShortestPathConstants =
  ShortestPathConstantsTarget |
  ShortestPathConstantsEdges |
  ShortestPathConstantsDistance |
  ShortestPathConstantsMaxDistance |
  ShortestPathConstantsIncludeEdges
  deriving (Eq, Ord, Read, Show)
_ShortestPathConstants = Core.Name "hydra.tinkerpop.gremlin.ShortestPathConstants"
_ShortestPathConstants_target = Core.Name "target"
_ShortestPathConstants_edges = Core.Name "edges"
_ShortestPathConstants_distance = Core.Name "distance"
_ShortestPathConstants_maxDistance = Core.Name "maxDistance"
_ShortestPathConstants_includeEdges = Core.Name "includeEdges"
data SplitArgs =
  SplitArgs {
    splitArgsScope :: (Maybe TraversalScopeArgument),
    splitArgsDelimiter :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)
_SplitArgs = Core.Name "hydra.tinkerpop.gremlin.SplitArgs"
_SplitArgs_scope = Core.Name "scope"
_SplitArgs_delimiter = Core.Name "delimiter"
data StringAndObject =
  StringAndObject {
    stringAndObjectKey :: StringArgument,
    stringAndObjectValue :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)
_StringAndObject = Core.Name "hydra.tinkerpop.gremlin.StringAndObject"
_StringAndObject_key = Core.Name "key"
_StringAndObject_value = Core.Name "value"
data StringAndOptionalObject =
  StringAndOptionalObject {
    stringAndOptionalObjectKey :: StringArgument,
    stringAndOptionalObjectValue :: (Maybe GenericLiteralArgument)}
  deriving (Eq, Ord, Read, Show)
_StringAndOptionalObject = Core.Name "hydra.tinkerpop.gremlin.StringAndOptionalObject"
_StringAndOptionalObject_key = Core.Name "key"
_StringAndOptionalObject_value = Core.Name "value"
data StringArgument =
  StringArgumentValue String |
  StringArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_StringArgument = Core.Name "hydra.tinkerpop.gremlin.StringArgument"
_StringArgument_value = Core.Name "value"
_StringArgument_variable = Core.Name "variable"
data StringArgumentOrNestedTraversal =
  StringArgumentOrNestedTraversalString StringArgument |
  StringArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_StringArgumentOrNestedTraversal = Core.Name "hydra.tinkerpop.gremlin.StringArgumentOrNestedTraversal"
_StringArgumentOrNestedTraversal_string = Core.Name "string"
_StringArgumentOrNestedTraversal_traversal = Core.Name "traversal"
data StringKeyAndObject =
  StringKeyAndObject {
    stringKeyAndObjectKey :: StringNullableArgument,
    stringKeyAndObjectObject :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)
_StringKeyAndObject = Core.Name "hydra.tinkerpop.gremlin.StringKeyAndObject"
_StringKeyAndObject_key = Core.Name "key"
_StringKeyAndObject_object = Core.Name "object"
data StringKeyAndPredicate =
  StringKeyAndPredicate {
    stringKeyAndPredicateKey :: StringNullableArgument,
    stringKeyAndPredicatePredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)
_StringKeyAndPredicate = Core.Name "hydra.tinkerpop.gremlin.StringKeyAndPredicate"
_StringKeyAndPredicate_key = Core.Name "key"
_StringKeyAndPredicate_predicate = Core.Name "predicate"
data StringNullableArgument =
  StringNullableArgumentValue (Maybe String) |
  StringNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_StringNullableArgument = Core.Name "hydra.tinkerpop.gremlin.StringNullableArgument"
_StringNullableArgument_value = Core.Name "value"
_StringNullableArgument_variable = Core.Name "variable"
data StringRange =
  StringRange {
    stringRangeLeft :: String,
    stringRangeRight :: String}
  deriving (Eq, Ord, Read, Show)
_StringRange = Core.Name "hydra.tinkerpop.gremlin.StringRange"
_StringRange_left = Core.Name "left"
_StringRange_right = Core.Name "right"
data StructureVertex =
  StructureVertex {
    structureVertexNew :: Bool,
    structureVertexId :: GenericLiteralArgument,
    structureVertexLabel :: StringArgument}
  deriving (Eq, Ord, Read, Show)
_StructureVertex = Core.Name "hydra.tinkerpop.gremlin.StructureVertex"
_StructureVertex_new = Core.Name "new"
_StructureVertex_id = Core.Name "id"
_StructureVertex_label = Core.Name "label"
data StructureVertexArgument =
  StructureVertexArgumentValue StructureVertex |
  StructureVertexArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_StructureVertexArgument = Core.Name "hydra.tinkerpop.gremlin.StructureVertexArgument"
_StructureVertexArgument_value = Core.Name "value"
_StructureVertexArgument_variable = Core.Name "variable"
data SubstringArgs =
  SubstringArgs {
    substringArgsScope :: (Maybe TraversalScopeArgument),
    substringArgsStart :: IntegerArgument,
    substringArgsEnd :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)
_SubstringArgs = Core.Name "hydra.tinkerpop.gremlin.SubstringArgs"
_SubstringArgs_scope = Core.Name "scope"
_SubstringArgs_start = Core.Name "start"
_SubstringArgs_end = Core.Name "end"
data TailArgs =
  TailArgs {
    tailArgsScope :: (Maybe TraversalScopeArgument),
    tailArgsInteger :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)
_TailArgs = Core.Name "hydra.tinkerpop.gremlin.TailArgs"
_TailArgs_scope = Core.Name "scope"
_TailArgs_integer = Core.Name "integer"
data TerminatedTraversal =
  TerminatedTraversal {
    terminatedTraversalRoot :: RootTraversal,
    terminatedTraversalTerminal :: TraversalTerminalMethod}
  deriving (Eq, Ord, Read, Show)
_TerminatedTraversal = Core.Name "hydra.tinkerpop.gremlin.TerminatedTraversal"
_TerminatedTraversal_root = Core.Name "root"
_TerminatedTraversal_terminal = Core.Name "terminal"
data ToArgs =
  ToArgsDirection DirectionAndVarargs |
  ToArgsString StringArgument |
  ToArgsVertex StructureVertexArgument |
  ToArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_ToArgs = Core.Name "hydra.tinkerpop.gremlin.ToArgs"
_ToArgs_direction = Core.Name "direction"
_ToArgs_string = Core.Name "string"
_ToArgs_vertex = Core.Name "vertex"
_ToArgs_traversal = Core.Name "traversal"
data TransactionPart =
  TransactionPartBegin |
  TransactionPartCommit |
  TransactionPartRollback
  deriving (Eq, Ord, Read, Show)
_TransactionPart = Core.Name "hydra.tinkerpop.gremlin.TransactionPart"
_TransactionPart_begin = Core.Name "begin"
_TransactionPart_commit = Core.Name "commit"
_TransactionPart_rollback = Core.Name "rollback"
data TraversalBiFunctionArgument =
  TraversalBiFunctionArgumentValue TraversalOperator |
  TraversalBiFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalBiFunctionArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalBiFunctionArgument"
_TraversalBiFunctionArgument_value = Core.Name "value"
_TraversalBiFunctionArgument_variable = Core.Name "variable"
data TraversalCardinality =
  TraversalCardinalitySingle GenericLiteral |
  TraversalCardinalitySet GenericLiteral |
  TraversalCardinalityList GenericLiteral
  deriving (Eq, Ord, Read, Show)
_TraversalCardinality = Core.Name "hydra.tinkerpop.gremlin.TraversalCardinality"
_TraversalCardinality_single = Core.Name "single"
_TraversalCardinality_set = Core.Name "set"
_TraversalCardinality_list = Core.Name "list"
data TraversalCardinalityArgument =
  TraversalCardinalityArgumentValue TraversalCardinality |
  TraversalCardinalityArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalCardinalityArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalCardinalityArgument"
_TraversalCardinalityArgument_value = Core.Name "value"
_TraversalCardinalityArgument_variable = Core.Name "variable"
data TraversalColumn =
  TraversalColumnKeys |
  TraversalColumnValues
  deriving (Eq, Ord, Read, Show)
_TraversalColumn = Core.Name "hydra.tinkerpop.gremlin.TraversalColumn"
_TraversalColumn_keys = Core.Name "keys"
_TraversalColumn_values = Core.Name "values"
data TraversalColumnArgument =
  TraversalColumnArgumentValue TraversalColumn |
  TraversalColumnArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalColumnArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalColumnArgument"
_TraversalColumnArgument_value = Core.Name "value"
_TraversalColumnArgument_variable = Core.Name "variable"
data TraversalComparatorArgument =
  TraversalComparatorArgumentValue TraversalOrder |
  TraversalComparatorArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalComparatorArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalComparatorArgument"
_TraversalComparatorArgument_value = Core.Name "value"
_TraversalComparatorArgument_variable = Core.Name "variable"
data TraversalDT =
  TraversalDTSecond |
  TraversalDTMinute |
  TraversalDTHour |
  TraversalDTDay
  deriving (Eq, Ord, Read, Show)
_TraversalDT = Core.Name "hydra.tinkerpop.gremlin.TraversalDT"
_TraversalDT_second = Core.Name "second"
_TraversalDT_minute = Core.Name "minute"
_TraversalDT_hour = Core.Name "hour"
_TraversalDT_day = Core.Name "day"
data TraversalDTArgument =
  TraversalDTArgumentValue TraversalDT |
  TraversalDTArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalDTArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalDTArgument"
_TraversalDTArgument_value = Core.Name "value"
_TraversalDTArgument_variable = Core.Name "variable"
data TraversalDirection =
  TraversalDirectionIn |
  TraversalDirectionOut |
  TraversalDirectionBoth |
  TraversalDirectionFrom |
  TraversalDirectionTo
  deriving (Eq, Ord, Read, Show)
_TraversalDirection = Core.Name "hydra.tinkerpop.gremlin.TraversalDirection"
_TraversalDirection_in = Core.Name "in"
_TraversalDirection_out = Core.Name "out"
_TraversalDirection_both = Core.Name "both"
_TraversalDirection_from = Core.Name "from"
_TraversalDirection_to = Core.Name "to"
data TraversalDirectionArgument =
  TraversalDirectionArgumentValue TraversalDirection |
  TraversalDirectionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalDirectionArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalDirectionArgument"
_TraversalDirectionArgument_value = Core.Name "value"
_TraversalDirectionArgument_variable = Core.Name "variable"
data TraversalFunction =
  TraversalFunctionToken TraversalToken |
  TraversalFunctionColumn TraversalColumn
  deriving (Eq, Ord, Read, Show)
_TraversalFunction = Core.Name "hydra.tinkerpop.gremlin.TraversalFunction"
_TraversalFunction_token = Core.Name "token"
_TraversalFunction_column = Core.Name "column"
data TraversalFunctionArgument =
  TraversalFunctionArgumentValue TraversalFunction |
  TraversalFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalFunctionArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalFunctionArgument"
_TraversalFunctionArgument_value = Core.Name "value"
_TraversalFunctionArgument_variable = Core.Name "variable"
data TraversalGType =
  TraversalGTypeBigDecimal |
  TraversalGTypeBigDecimalU |
  TraversalGTypeBigInt |
  TraversalGTypeBigIntU |
  TraversalGTypeBinary |
  TraversalGTypeBinaryU |
  TraversalGTypeBoolean |
  TraversalGTypeBooleanU |
  TraversalGTypeByte |
  TraversalGTypeByteU |
  TraversalGTypeChar |
  TraversalGTypeCharU |
  TraversalGTypeDateTime |
  TraversalGTypeDateTimeU |
  TraversalGTypeDouble |
  TraversalGTypeDoubleU |
  TraversalGTypeDuration |
  TraversalGTypeDurationU |
  TraversalGTypeEdge |
  TraversalGTypeEdgeU |
  TraversalGTypeFloat |
  TraversalGTypeFloatU |
  TraversalGTypeGraph |
  TraversalGTypeGraphU |
  TraversalGTypeInt |
  TraversalGTypeIntU |
  TraversalGTypeList |
  TraversalGTypeListU |
  TraversalGTypeLong |
  TraversalGTypeLongU |
  TraversalGTypeMap |
  TraversalGTypeMapU |
  TraversalGTypeNull |
  TraversalGTypeNullU |
  TraversalGTypeNumber |
  TraversalGTypeNumberU |
  TraversalGTypePath |
  TraversalGTypePathU |
  TraversalGTypeProperty |
  TraversalGTypePropertyU |
  TraversalGTypeSet |
  TraversalGTypeSetU |
  TraversalGTypeShort |
  TraversalGTypeShortU |
  TraversalGTypeString |
  TraversalGTypeStringU |
  TraversalGTypeTree |
  TraversalGTypeTreeU |
  TraversalGTypeUuid |
  TraversalGTypeUuidL |
  TraversalGTypeVertex |
  TraversalGTypeVertexU |
  TraversalGTypeVproperty |
  TraversalGTypeVpropertyU
  deriving (Eq, Ord, Read, Show)
_TraversalGType = Core.Name "hydra.tinkerpop.gremlin.TraversalGType"
_TraversalGType_bigDecimal = Core.Name "bigDecimal"
_TraversalGType_bigDecimalU = Core.Name "bigDecimalU"
_TraversalGType_bigInt = Core.Name "bigInt"
_TraversalGType_bigIntU = Core.Name "bigIntU"
_TraversalGType_binary = Core.Name "binary"
_TraversalGType_binaryU = Core.Name "binaryU"
_TraversalGType_boolean = Core.Name "boolean"
_TraversalGType_booleanU = Core.Name "booleanU"
_TraversalGType_byte = Core.Name "byte"
_TraversalGType_byteU = Core.Name "byteU"
_TraversalGType_char = Core.Name "char"
_TraversalGType_charU = Core.Name "charU"
_TraversalGType_dateTime = Core.Name "dateTime"
_TraversalGType_dateTimeU = Core.Name "dateTimeU"
_TraversalGType_double = Core.Name "double"
_TraversalGType_doubleU = Core.Name "doubleU"
_TraversalGType_duration = Core.Name "duration"
_TraversalGType_durationU = Core.Name "durationU"
_TraversalGType_edge = Core.Name "edge"
_TraversalGType_edgeU = Core.Name "edgeU"
_TraversalGType_float = Core.Name "float"
_TraversalGType_floatU = Core.Name "floatU"
_TraversalGType_graph = Core.Name "graph"
_TraversalGType_graphU = Core.Name "graphU"
_TraversalGType_int = Core.Name "int"
_TraversalGType_intU = Core.Name "intU"
_TraversalGType_list = Core.Name "list"
_TraversalGType_listU = Core.Name "listU"
_TraversalGType_long = Core.Name "long"
_TraversalGType_longU = Core.Name "longU"
_TraversalGType_map = Core.Name "map"
_TraversalGType_mapU = Core.Name "mapU"
_TraversalGType_null = Core.Name "null"
_TraversalGType_nullU = Core.Name "nullU"
_TraversalGType_number = Core.Name "number"
_TraversalGType_numberU = Core.Name "numberU"
_TraversalGType_path = Core.Name "path"
_TraversalGType_pathU = Core.Name "pathU"
_TraversalGType_property = Core.Name "property"
_TraversalGType_propertyU = Core.Name "propertyU"
_TraversalGType_set = Core.Name "set"
_TraversalGType_setU = Core.Name "setU"
_TraversalGType_short = Core.Name "short"
_TraversalGType_shortU = Core.Name "shortU"
_TraversalGType_string = Core.Name "string"
_TraversalGType_stringU = Core.Name "stringU"
_TraversalGType_tree = Core.Name "tree"
_TraversalGType_treeU = Core.Name "treeU"
_TraversalGType_uuid = Core.Name "uuid"
_TraversalGType_uuidL = Core.Name "uuidL"
_TraversalGType_vertex = Core.Name "vertex"
_TraversalGType_vertexU = Core.Name "vertexU"
_TraversalGType_vproperty = Core.Name "vproperty"
_TraversalGType_vpropertyU = Core.Name "vpropertyU"
data TraversalGTypeArgument =
  TraversalGTypeArgumentValue TraversalGType |
  TraversalGTypeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalGTypeArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalGTypeArgument"
_TraversalGTypeArgument_value = Core.Name "value"
_TraversalGTypeArgument_variable = Core.Name "variable"
data TraversalMerge =
  TraversalMergeOnCreate |
  TraversalMergeOnMatch |
  TraversalMergeOutV |
  TraversalMergeInV
  deriving (Eq, Ord, Read, Show)
_TraversalMerge = Core.Name "hydra.tinkerpop.gremlin.TraversalMerge"
_TraversalMerge_onCreate = Core.Name "onCreate"
_TraversalMerge_onMatch = Core.Name "onMatch"
_TraversalMerge_outV = Core.Name "outV"
_TraversalMerge_inV = Core.Name "inV"
data TraversalMergeArgument =
  TraversalMergeArgumentValue TraversalMerge |
  TraversalMergeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalMergeArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalMergeArgument"
_TraversalMergeArgument_value = Core.Name "value"
_TraversalMergeArgument_variable = Core.Name "variable"
data TraversalMethod =
  TraversalMethodV [GenericLiteralArgument] |
  TraversalMethodE [GenericLiteralArgument] |
  TraversalMethodAddE StringArgumentOrNestedTraversal |
  TraversalMethodAddV (Maybe StringArgumentOrNestedTraversal) |
  TraversalMethodMergeE MergeArgs |
  TraversalMethodMergeV MergeArgs |
  TraversalMethodAggregate StringArgument |
  TraversalMethodAll TraversalPredicate |
  TraversalMethodAnd [NestedTraversal] |
  TraversalMethodAny TraversalPredicate |
  TraversalMethodAs AsArgs |
  TraversalMethodAsBool |
  TraversalMethodAsDate |
  TraversalMethodAsNumber (Maybe TraversalGTypeArgument) |
  TraversalMethodAsString (Maybe TraversalScopeArgument) |
  TraversalMethodBarrier (Maybe BarrierArgs) |
  TraversalMethodBoth [StringNullableArgument] |
  TraversalMethodBothE [StringNullableArgument] |
  TraversalMethodBothV |
  TraversalMethodBranch NestedTraversal |
  TraversalMethodBy ByArgs |
  TraversalMethodCall ServiceCall |
  TraversalMethodCap AsArgs |
  TraversalMethodChoose ChooseArgs |
  TraversalMethodCoalesce [NestedTraversal] |
  TraversalMethodCoin NumericArgument |
  TraversalMethodCombine GenericLiteralArgument |
  TraversalMethodConcat ConcatArgs |
  TraversalMethodConjoin StringArgument |
  TraversalMethodConnectedComponent |
  TraversalMethodConstant GenericLiteralArgument |
  TraversalMethodCount (Maybe TraversalScopeArgument) |
  TraversalMethodCyclicPath |
  TraversalMethodDateAdd DateAddArgs |
  TraversalMethodDateDiff DateDiffArgs |
  TraversalMethodDedup DedupArgs |
  TraversalMethodDifference GenericLiteralArgument |
  TraversalMethodDiscard |
  TraversalMethodDisjunct GenericLiteralArgument |
  TraversalMethodDrop |
  TraversalMethodElement [StringNullableArgument] |
  TraversalMethodElementMap [StringNullableArgument] |
  TraversalMethodEmit (Maybe PredicateOrTraversal) |
  TraversalMethodFail (Maybe StringArgument) |
  TraversalMethodFilter PredicateOrTraversal |
  TraversalMethodFlatMap NestedTraversal |
  TraversalMethodFold (Maybe FoldArgs) |
  TraversalMethodFormat StringArgument |
  TraversalMethodFrom FromArgs |
  TraversalMethodGroup (Maybe StringArgument) |
  TraversalMethodGroupCount (Maybe StringArgument) |
  TraversalMethodHas HasArgs |
  TraversalMethodHasId PredicateOrObjects |
  TraversalMethodHasKey PredicateOrStrings |
  TraversalMethodHasLabel PredicateOrStrings |
  TraversalMethodHasNot StringNullableArgument |
  TraversalMethodHasValue PredicateOrObjects |
  TraversalMethodId |
  TraversalMethodIdentity |
  TraversalMethodIn [StringNullableArgument] |
  TraversalMethodInE [StringNullableArgument] |
  TraversalMethodInV |
  TraversalMethodIndex |
  TraversalMethodInject [GenericLiteralArgument] |
  TraversalMethodIntersect GenericLiteralArgument |
  TraversalMethodIs PredicateOrObject |
  TraversalMethodKey |
  TraversalMethodLabel |
  TraversalMethodLength (Maybe TraversalScopeArgument) |
  TraversalMethodLimit ScopeAndInteger |
  TraversalMethodLocal NestedTraversal |
  TraversalMethodLoops (Maybe StringArgument) |
  TraversalMethodLTrim (Maybe TraversalScopeArgument) |
  TraversalMethodMap NestedTraversal |
  TraversalMethodMatch [NestedTraversal] |
  TraversalMethodMath StringArgument |
  TraversalMethodMax (Maybe TraversalScopeArgument) |
  TraversalMethodMean (Maybe TraversalScopeArgument) |
  TraversalMethodMerge GenericLiteralArgument |
  TraversalMethodMin (Maybe TraversalScopeArgument) |
  TraversalMethodNone TraversalPredicate |
  TraversalMethodNot NestedTraversal |
  TraversalMethodOption OptionArgs |
  TraversalMethodOptional NestedTraversal |
  TraversalMethodOr [NestedTraversal] |
  TraversalMethodOrder (Maybe TraversalScopeArgument) |
  TraversalMethodOtherV |
  TraversalMethodOut [StringNullableArgument] |
  TraversalMethodOutE [StringNullableArgument] |
  TraversalMethodOutV |
  TraversalMethodPageRank (Maybe NumericArgument) |
  TraversalMethodPath |
  TraversalMethodPeerPressure |
  TraversalMethodProduct GenericLiteralArgument |
  TraversalMethodProfile (Maybe StringArgument) |
  TraversalMethodProject AsArgs |
  TraversalMethodProperties [StringNullableArgument] |
  TraversalMethodProperty PropertyArgs |
  TraversalMethodPropertyMap [StringNullableArgument] |
  TraversalMethodRange RangeArgs |
  TraversalMethodRead |
  TraversalMethodRepeat RepeatArgs |
  TraversalMethodReplace ReplaceArgs |
  TraversalMethodReverse |
  TraversalMethodRTrim (Maybe TraversalScopeArgument) |
  TraversalMethodSack (Maybe TraversalBiFunctionArgument) |
  TraversalMethodSample SampleByScope |
  TraversalMethodSelect SelectArgs |
  TraversalMethodShortestPath |
  TraversalMethodSideEffect NestedTraversal |
  TraversalMethodSimplePath |
  TraversalMethodSkip ScopeAndInteger |
  TraversalMethodSplit SplitArgs |
  TraversalMethodSubgraph StringArgument |
  TraversalMethodSubstring SubstringArgs |
  TraversalMethodSum (Maybe TraversalScopeArgument) |
  TraversalMethodTail (Maybe TailArgs) |
  TraversalMethodTimeLimit IntegerArgument |
  TraversalMethodTimes IntegerArgument |
  TraversalMethodTo ToArgs |
  TraversalMethodToE DirectionAndVarargs |
  TraversalMethodToLower (Maybe TraversalScopeArgument) |
  TraversalMethodToUpper (Maybe TraversalScopeArgument) |
  TraversalMethodToV TraversalDirectionArgument |
  TraversalMethodTree (Maybe StringArgument) |
  TraversalMethodTrim (Maybe TraversalScopeArgument) |
  TraversalMethodUnfold |
  TraversalMethodUnion [NestedTraversal] |
  TraversalMethodUntil PredicateOrTraversal |
  TraversalMethodValue |
  TraversalMethodValueMap ValueMapArgs |
  TraversalMethodValues [StringNullableArgument] |
  TraversalMethodWhere WhereArgs |
  TraversalMethodWith WithArgs |
  TraversalMethodWrite
  deriving (Eq, Ord, Read, Show)
_TraversalMethod = Core.Name "hydra.tinkerpop.gremlin.TraversalMethod"
_TraversalMethod_v = Core.Name "v"
_TraversalMethod_e = Core.Name "e"
_TraversalMethod_addE = Core.Name "addE"
_TraversalMethod_addV = Core.Name "addV"
_TraversalMethod_mergeE = Core.Name "mergeE"
_TraversalMethod_mergeV = Core.Name "mergeV"
_TraversalMethod_aggregate = Core.Name "aggregate"
_TraversalMethod_all = Core.Name "all"
_TraversalMethod_and = Core.Name "and"
_TraversalMethod_any = Core.Name "any"
_TraversalMethod_as = Core.Name "as"
_TraversalMethod_asBool = Core.Name "asBool"
_TraversalMethod_asDate = Core.Name "asDate"
_TraversalMethod_asNumber = Core.Name "asNumber"
_TraversalMethod_asString = Core.Name "asString"
_TraversalMethod_barrier = Core.Name "barrier"
_TraversalMethod_both = Core.Name "both"
_TraversalMethod_bothE = Core.Name "bothE"
_TraversalMethod_bothV = Core.Name "bothV"
_TraversalMethod_branch = Core.Name "branch"
_TraversalMethod_by = Core.Name "by"
_TraversalMethod_call = Core.Name "call"
_TraversalMethod_cap = Core.Name "cap"
_TraversalMethod_choose = Core.Name "choose"
_TraversalMethod_coalesce = Core.Name "coalesce"
_TraversalMethod_coin = Core.Name "coin"
_TraversalMethod_combine = Core.Name "combine"
_TraversalMethod_concat = Core.Name "concat"
_TraversalMethod_conjoin = Core.Name "conjoin"
_TraversalMethod_connectedComponent = Core.Name "connectedComponent"
_TraversalMethod_constant = Core.Name "constant"
_TraversalMethod_count = Core.Name "count"
_TraversalMethod_cyclicPath = Core.Name "cyclicPath"
_TraversalMethod_dateAdd = Core.Name "dateAdd"
_TraversalMethod_dateDiff = Core.Name "dateDiff"
_TraversalMethod_dedup = Core.Name "dedup"
_TraversalMethod_difference = Core.Name "difference"
_TraversalMethod_discard = Core.Name "discard"
_TraversalMethod_disjunct = Core.Name "disjunct"
_TraversalMethod_drop = Core.Name "drop"
_TraversalMethod_element = Core.Name "element"
_TraversalMethod_elementMap = Core.Name "elementMap"
_TraversalMethod_emit = Core.Name "emit"
_TraversalMethod_fail = Core.Name "fail"
_TraversalMethod_filter = Core.Name "filter"
_TraversalMethod_flatMap = Core.Name "flatMap"
_TraversalMethod_fold = Core.Name "fold"
_TraversalMethod_format = Core.Name "format"
_TraversalMethod_from = Core.Name "from"
_TraversalMethod_group = Core.Name "group"
_TraversalMethod_groupCount = Core.Name "groupCount"
_TraversalMethod_has = Core.Name "has"
_TraversalMethod_hasId = Core.Name "hasId"
_TraversalMethod_hasKey = Core.Name "hasKey"
_TraversalMethod_hasLabel = Core.Name "hasLabel"
_TraversalMethod_hasNot = Core.Name "hasNot"
_TraversalMethod_hasValue = Core.Name "hasValue"
_TraversalMethod_id = Core.Name "id"
_TraversalMethod_identity = Core.Name "identity"
_TraversalMethod_in = Core.Name "in"
_TraversalMethod_inE = Core.Name "inE"
_TraversalMethod_inV = Core.Name "inV"
_TraversalMethod_index = Core.Name "index"
_TraversalMethod_inject = Core.Name "inject"
_TraversalMethod_intersect = Core.Name "intersect"
_TraversalMethod_is = Core.Name "is"
_TraversalMethod_key = Core.Name "key"
_TraversalMethod_label = Core.Name "label"
_TraversalMethod_length = Core.Name "length"
_TraversalMethod_limit = Core.Name "limit"
_TraversalMethod_local = Core.Name "local"
_TraversalMethod_loops = Core.Name "loops"
_TraversalMethod_lTrim = Core.Name "lTrim"
_TraversalMethod_map = Core.Name "map"
_TraversalMethod_match = Core.Name "match"
_TraversalMethod_math = Core.Name "math"
_TraversalMethod_max = Core.Name "max"
_TraversalMethod_mean = Core.Name "mean"
_TraversalMethod_merge = Core.Name "merge"
_TraversalMethod_min = Core.Name "min"
_TraversalMethod_none = Core.Name "none"
_TraversalMethod_not = Core.Name "not"
_TraversalMethod_option = Core.Name "option"
_TraversalMethod_optional = Core.Name "optional"
_TraversalMethod_or = Core.Name "or"
_TraversalMethod_order = Core.Name "order"
_TraversalMethod_otherV = Core.Name "otherV"
_TraversalMethod_out = Core.Name "out"
_TraversalMethod_outE = Core.Name "outE"
_TraversalMethod_outV = Core.Name "outV"
_TraversalMethod_pageRank = Core.Name "pageRank"
_TraversalMethod_path = Core.Name "path"
_TraversalMethod_peerPressure = Core.Name "peerPressure"
_TraversalMethod_product = Core.Name "product"
_TraversalMethod_profile = Core.Name "profile"
_TraversalMethod_project = Core.Name "project"
_TraversalMethod_properties = Core.Name "properties"
_TraversalMethod_property = Core.Name "property"
_TraversalMethod_propertyMap = Core.Name "propertyMap"
_TraversalMethod_range = Core.Name "range"
_TraversalMethod_read = Core.Name "read"
_TraversalMethod_repeat = Core.Name "repeat"
_TraversalMethod_replace = Core.Name "replace"
_TraversalMethod_reverse = Core.Name "reverse"
_TraversalMethod_rTrim = Core.Name "rTrim"
_TraversalMethod_sack = Core.Name "sack"
_TraversalMethod_sample = Core.Name "sample"
_TraversalMethod_select = Core.Name "select"
_TraversalMethod_shortestPath = Core.Name "shortestPath"
_TraversalMethod_sideEffect = Core.Name "sideEffect"
_TraversalMethod_simplePath = Core.Name "simplePath"
_TraversalMethod_skip = Core.Name "skip"
_TraversalMethod_split = Core.Name "split"
_TraversalMethod_subgraph = Core.Name "subgraph"
_TraversalMethod_substring = Core.Name "substring"
_TraversalMethod_sum = Core.Name "sum"
_TraversalMethod_tail = Core.Name "tail"
_TraversalMethod_timeLimit = Core.Name "timeLimit"
_TraversalMethod_times = Core.Name "times"
_TraversalMethod_to = Core.Name "to"
_TraversalMethod_toE = Core.Name "toE"
_TraversalMethod_toLower = Core.Name "toLower"
_TraversalMethod_toUpper = Core.Name "toUpper"
_TraversalMethod_toV = Core.Name "toV"
_TraversalMethod_tree = Core.Name "tree"
_TraversalMethod_trim = Core.Name "trim"
_TraversalMethod_unfold = Core.Name "unfold"
_TraversalMethod_union = Core.Name "union"
_TraversalMethod_until = Core.Name "until"
_TraversalMethod_value = Core.Name "value"
_TraversalMethod_valueMap = Core.Name "valueMap"
_TraversalMethod_values = Core.Name "values"
_TraversalMethod_where = Core.Name "where"
_TraversalMethod_with = Core.Name "with"
_TraversalMethod_write = Core.Name "write"
data TraversalOperator =
  TraversalOperatorAddAll |
  TraversalOperatorAnd |
  TraversalOperatorAssign |
  TraversalOperatorDiv |
  TraversalOperatorMax |
  TraversalOperatorMin |
  TraversalOperatorMinus |
  TraversalOperatorMult |
  TraversalOperatorOr |
  TraversalOperatorSum |
  TraversalOperatorSumLong
  deriving (Eq, Ord, Read, Show)
_TraversalOperator = Core.Name "hydra.tinkerpop.gremlin.TraversalOperator"
_TraversalOperator_addAll = Core.Name "addAll"
_TraversalOperator_and = Core.Name "and"
_TraversalOperator_assign = Core.Name "assign"
_TraversalOperator_div = Core.Name "div"
_TraversalOperator_max = Core.Name "max"
_TraversalOperator_min = Core.Name "min"
_TraversalOperator_minus = Core.Name "minus"
_TraversalOperator_mult = Core.Name "mult"
_TraversalOperator_or = Core.Name "or"
_TraversalOperator_sum = Core.Name "sum"
_TraversalOperator_sumLong = Core.Name "sumLong"
data TraversalOrder =
  TraversalOrderAsc |
  TraversalOrderDesc |
  TraversalOrderShuffle
  deriving (Eq, Ord, Read, Show)
_TraversalOrder = Core.Name "hydra.tinkerpop.gremlin.TraversalOrder"
_TraversalOrder_asc = Core.Name "asc"
_TraversalOrder_desc = Core.Name "desc"
_TraversalOrder_shuffle = Core.Name "shuffle"
data TraversalOrderArgument =
  TraversalOrderArgumentValue TraversalOrder |
  TraversalOrderArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalOrderArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalOrderArgument"
_TraversalOrderArgument_value = Core.Name "value"
_TraversalOrderArgument_variable = Core.Name "variable"
data TraversalPick =
  TraversalPickAny |
  TraversalPickNone |
  TraversalPickUnproductive
  deriving (Eq, Ord, Read, Show)
_TraversalPick = Core.Name "hydra.tinkerpop.gremlin.TraversalPick"
_TraversalPick_any = Core.Name "any"
_TraversalPick_none = Core.Name "none"
_TraversalPick_unproductive = Core.Name "unproductive"
data TraversalPop =
  TraversalPopFirst |
  TraversalPopLast |
  TraversalPopAll |
  TraversalPopMixed
  deriving (Eq, Ord, Read, Show)
_TraversalPop = Core.Name "hydra.tinkerpop.gremlin.TraversalPop"
_TraversalPop_first = Core.Name "first"
_TraversalPop_last = Core.Name "last"
_TraversalPop_all = Core.Name "all"
_TraversalPop_mixed = Core.Name "mixed"
data TraversalPopArgument =
  TraversalPopArgumentValue TraversalPop |
  TraversalPopArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalPopArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalPopArgument"
_TraversalPopArgument_value = Core.Name "value"
_TraversalPopArgument_variable = Core.Name "variable"
data TraversalPredicate =
  TraversalPredicateEq GenericLiteralArgument |
  TraversalPredicateNeq GenericLiteralArgument |
  TraversalPredicateLt GenericLiteralArgument |
  TraversalPredicateLte GenericLiteralArgument |
  TraversalPredicateGt GenericLiteralArgument |
  TraversalPredicateGte GenericLiteralArgument |
  TraversalPredicateInside RangeArgument |
  TraversalPredicateOutside RangeArgument |
  TraversalPredicateBetween RangeArgument |
  TraversalPredicateWithin (Maybe GenericLiteralArgument) |
  TraversalPredicateWithout (Maybe GenericLiteralArgument) |
  TraversalPredicateNot TraversalPredicate |
  TraversalPredicateStartingWith StringArgument |
  TraversalPredicateNotStartingWith StringArgument |
  TraversalPredicateEndingWith StringArgument |
  TraversalPredicateNotEndingWith StringArgument |
  TraversalPredicateContaining StringArgument |
  TraversalPredicateNotContaining StringArgument |
  TraversalPredicateRegex StringArgument |
  TraversalPredicateNotRegex StringArgument |
  TraversalPredicateTypeOf TypeOfArg |
  TraversalPredicateAnd TwoTraversalPredicates |
  TraversalPredicateOr TwoTraversalPredicates |
  TraversalPredicateNegate TraversalPredicate
  deriving (Eq, Ord, Read, Show)
_TraversalPredicate = Core.Name "hydra.tinkerpop.gremlin.TraversalPredicate"
_TraversalPredicate_eq = Core.Name "eq"
_TraversalPredicate_neq = Core.Name "neq"
_TraversalPredicate_lt = Core.Name "lt"
_TraversalPredicate_lte = Core.Name "lte"
_TraversalPredicate_gt = Core.Name "gt"
_TraversalPredicate_gte = Core.Name "gte"
_TraversalPredicate_inside = Core.Name "inside"
_TraversalPredicate_outside = Core.Name "outside"
_TraversalPredicate_between = Core.Name "between"
_TraversalPredicate_within = Core.Name "within"
_TraversalPredicate_without = Core.Name "without"
_TraversalPredicate_not = Core.Name "not"
_TraversalPredicate_startingWith = Core.Name "startingWith"
_TraversalPredicate_notStartingWith = Core.Name "notStartingWith"
_TraversalPredicate_endingWith = Core.Name "endingWith"
_TraversalPredicate_notEndingWith = Core.Name "notEndingWith"
_TraversalPredicate_containing = Core.Name "containing"
_TraversalPredicate_notContaining = Core.Name "notContaining"
_TraversalPredicate_regex = Core.Name "regex"
_TraversalPredicate_notRegex = Core.Name "notRegex"
_TraversalPredicate_typeOf = Core.Name "typeOf"
_TraversalPredicate_and = Core.Name "and"
_TraversalPredicate_or = Core.Name "or"
_TraversalPredicate_negate = Core.Name "negate"
data TraversalSackMethodArgument =
  TraversalSackMethodArgumentValue |
  TraversalSackMethodArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalSackMethodArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalSackMethodArgument"
_TraversalSackMethodArgument_value = Core.Name "value"
_TraversalSackMethodArgument_variable = Core.Name "variable"
data TraversalScope =
  TraversalScopeLocal |
  TraversalScopeGlobal
  deriving (Eq, Ord, Read, Show)
_TraversalScope = Core.Name "hydra.tinkerpop.gremlin.TraversalScope"
_TraversalScope_local = Core.Name "local"
_TraversalScope_global = Core.Name "global"
data TraversalScopeArgument =
  TraversalScopeArgumentValue TraversalScope |
  TraversalScopeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalScopeArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalScopeArgument"
_TraversalScopeArgument_value = Core.Name "value"
_TraversalScopeArgument_variable = Core.Name "variable"
newtype TraversalSource =
  TraversalSource {
    unTraversalSource :: [TraversalSourceSelfMethod]}
  deriving (Eq, Ord, Read, Show)
_TraversalSource = Core.Name "hydra.tinkerpop.gremlin.TraversalSource"
data TraversalSourceQuery =
  TraversalSourceQuery {
    traversalSourceQuerySource :: TraversalSource,
    traversalSourceQueryTransactionPart :: (Maybe TransactionPart)}
  deriving (Eq, Ord, Read, Show)
_TraversalSourceQuery = Core.Name "hydra.tinkerpop.gremlin.TraversalSourceQuery"
_TraversalSourceQuery_source = Core.Name "source"
_TraversalSourceQuery_transactionPart = Core.Name "transactionPart"
data TraversalSourceSelfMethod =
  TraversalSourceSelfMethodWithBulk Bool |
  TraversalSourceSelfMethodWithPath |
  TraversalSourceSelfMethodWithSack WithSackArgs |
  TraversalSourceSelfMethodWithSideEffect StringAndObject |
  TraversalSourceSelfMethodWithStrategies [TraversalStrategy] |
  TraversalSourceSelfMethodWithoutStrategies [Identifier] |
  TraversalSourceSelfMethodWith StringAndOptionalObject
  deriving (Eq, Ord, Read, Show)
_TraversalSourceSelfMethod = Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSelfMethod"
_TraversalSourceSelfMethod_withBulk = Core.Name "withBulk"
_TraversalSourceSelfMethod_withPath = Core.Name "withPath"
_TraversalSourceSelfMethod_withSack = Core.Name "withSack"
_TraversalSourceSelfMethod_withSideEffect = Core.Name "withSideEffect"
_TraversalSourceSelfMethod_withStrategies = Core.Name "withStrategies"
_TraversalSourceSelfMethod_withoutStrategies = Core.Name "withoutStrategies"
_TraversalSourceSelfMethod_with = Core.Name "with"
data TraversalSourceSpawnMethod =
  TraversalSourceSpawnMethodAddE StringArgumentOrNestedTraversal |
  TraversalSourceSpawnMethodAddV (Maybe StringArgumentOrNestedTraversal) |
  TraversalSourceSpawnMethodE [GenericLiteralArgument] |
  TraversalSourceSpawnMethodV [GenericLiteralArgument] |
  TraversalSourceSpawnMethodMergeV MergeArgs |
  TraversalSourceSpawnMethodMergeE MergeArgs |
  TraversalSourceSpawnMethodInject [GenericLiteralArgument] |
  TraversalSourceSpawnMethodIo StringArgument |
  TraversalSourceSpawnMethodCall (Maybe ServiceCall) |
  TraversalSourceSpawnMethodUnion [NestedTraversal]
  deriving (Eq, Ord, Read, Show)
_TraversalSourceSpawnMethod = Core.Name "hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod"
_TraversalSourceSpawnMethod_addE = Core.Name "addE"
_TraversalSourceSpawnMethod_addV = Core.Name "addV"
_TraversalSourceSpawnMethod_e = Core.Name "e"
_TraversalSourceSpawnMethod_v = Core.Name "v"
_TraversalSourceSpawnMethod_mergeV = Core.Name "mergeV"
_TraversalSourceSpawnMethod_mergeE = Core.Name "mergeE"
_TraversalSourceSpawnMethod_inject = Core.Name "inject"
_TraversalSourceSpawnMethod_io = Core.Name "io"
_TraversalSourceSpawnMethod_call = Core.Name "call"
_TraversalSourceSpawnMethod_union = Core.Name "union"
data TraversalStrategy =
  TraversalStrategy {
    traversalStrategyNew :: Bool,
    traversalStrategyClass :: Identifier,
    traversalStrategyConfigurations :: [Configuration]}
  deriving (Eq, Ord, Read, Show)
_TraversalStrategy = Core.Name "hydra.tinkerpop.gremlin.TraversalStrategy"
_TraversalStrategy_new = Core.Name "new"
_TraversalStrategy_class = Core.Name "class"
_TraversalStrategy_configurations = Core.Name "configurations"
data TraversalTerminalMethod =
  TraversalTerminalMethodExplain |
  TraversalTerminalMethodIterate |
  TraversalTerminalMethodHasNext |
  TraversalTerminalMethodTryNext |
  TraversalTerminalMethodNext (Maybe IntegerLiteral) |
  TraversalTerminalMethodToList |
  TraversalTerminalMethodToSet |
  TraversalTerminalMethodToBulkSet
  deriving (Eq, Ord, Read, Show)
_TraversalTerminalMethod = Core.Name "hydra.tinkerpop.gremlin.TraversalTerminalMethod"
_TraversalTerminalMethod_explain = Core.Name "explain"
_TraversalTerminalMethod_iterate = Core.Name "iterate"
_TraversalTerminalMethod_hasNext = Core.Name "hasNext"
_TraversalTerminalMethod_tryNext = Core.Name "tryNext"
_TraversalTerminalMethod_next = Core.Name "next"
_TraversalTerminalMethod_toList = Core.Name "toList"
_TraversalTerminalMethod_toSet = Core.Name "toSet"
_TraversalTerminalMethod_toBulkSet = Core.Name "toBulkSet"
data TraversalToken =
  TraversalTokenId |
  TraversalTokenLabel |
  TraversalTokenKey |
  TraversalTokenValue
  deriving (Eq, Ord, Read, Show)
_TraversalToken = Core.Name "hydra.tinkerpop.gremlin.TraversalToken"
_TraversalToken_id = Core.Name "id"
_TraversalToken_label = Core.Name "label"
_TraversalToken_key = Core.Name "key"
_TraversalToken_value = Core.Name "value"
data TraversalTokenArgument =
  TraversalTokenArgumentValue TraversalToken |
  TraversalTokenArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)
_TraversalTokenArgument = Core.Name "hydra.tinkerpop.gremlin.TraversalTokenArgument"
_TraversalTokenArgument_value = Core.Name "value"
_TraversalTokenArgument_variable = Core.Name "variable"
data TwoTraversalPredicates =
  TwoTraversalPredicates {
    twoTraversalPredicatesLeft :: TraversalPredicate,
    twoTraversalPredicatesRight :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)
_TwoTraversalPredicates = Core.Name "hydra.tinkerpop.gremlin.TwoTraversalPredicates"
_TwoTraversalPredicates_left = Core.Name "left"
_TwoTraversalPredicates_right = Core.Name "right"
data TypeOfArg =
  TypeOfArgGType TraversalGType |
  TypeOfArgString StringArgument
  deriving (Eq, Ord, Read, Show)
_TypeOfArg = Core.Name "hydra.tinkerpop.gremlin.TypeOfArg"
_TypeOfArg_gType = Core.Name "gType"
_TypeOfArg_string = Core.Name "string"
data ValueMapArgs =
  ValueMapArgsString [StringNullableArgument] |
  ValueMapArgsBoolean ValueMapBooleanArgs
  deriving (Eq, Ord, Read, Show)
_ValueMapArgs = Core.Name "hydra.tinkerpop.gremlin.ValueMapArgs"
_ValueMapArgs_string = Core.Name "string"
_ValueMapArgs_boolean = Core.Name "boolean"
data ValueMapBooleanArgs =
  ValueMapBooleanArgs {
    valueMapBooleanArgsValue :: BooleanArgument,
    valueMapBooleanArgsKeys :: (Maybe [StringNullableArgument])}
  deriving (Eq, Ord, Read, Show)
_ValueMapBooleanArgs = Core.Name "hydra.tinkerpop.gremlin.ValueMapBooleanArgs"
_ValueMapBooleanArgs_value = Core.Name "value"
_ValueMapBooleanArgs_keys = Core.Name "keys"
data WhereArgs =
  WhereArgsPredicate WhereWithPredicateArgs |
  WhereArgsString StringArgument |
  WhereArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)
_WhereArgs = Core.Name "hydra.tinkerpop.gremlin.WhereArgs"
_WhereArgs_predicate = Core.Name "predicate"
_WhereArgs_string = Core.Name "string"
_WhereArgs_traversal = Core.Name "traversal"
data WhereWithPredicateArgs =
  WhereWithPredicateArgs {
    whereWithPredicateArgsLeftArg :: (Maybe StringArgument),
    whereWithPredicateArgsPredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)
_WhereWithPredicateArgs = Core.Name "hydra.tinkerpop.gremlin.WhereWithPredicateArgs"
_WhereWithPredicateArgs_leftArg = Core.Name "leftArg"
_WhereWithPredicateArgs_predicate = Core.Name "predicate"
data WithArgs =
  WithArgs {
    withArgsKeys :: WithArgsKeys,
    withArgsValues :: (Maybe WithArgsValues)}
  deriving (Eq, Ord, Read, Show)
_WithArgs = Core.Name "hydra.tinkerpop.gremlin.WithArgs"
_WithArgs_keys = Core.Name "keys"
_WithArgs_values = Core.Name "values"
data WithArgsKeys =
  WithArgsKeysWithOption WithOptionKeys |
  WithArgsKeysString StringArgument
  deriving (Eq, Ord, Read, Show)
_WithArgsKeys = Core.Name "hydra.tinkerpop.gremlin.WithArgsKeys"
_WithArgsKeys_withOption = Core.Name "withOption"
_WithArgsKeys_string = Core.Name "string"
data WithArgsValues =
  WithArgsValuesWithOptions WithOptionsValues |
  WithArgsValuesIo IoOptionsValues |
  WithArgsValuesObject GenericLiteralArgument
  deriving (Eq, Ord, Read, Show)
_WithArgsValues = Core.Name "hydra.tinkerpop.gremlin.WithArgsValues"
_WithArgsValues_withOptions = Core.Name "withOptions"
_WithArgsValues_io = Core.Name "io"
_WithArgsValues_object = Core.Name "object"
data WithOptionKeys =
  WithOptionKeysShortestPath ShortestPathConstants |
  WithOptionKeysConnectedComponent ConnectedComponentConstants |
  WithOptionKeysPageRank PageRankConstants |
  WithOptionKeysPeerPressure PeerPressureConstants |
  WithOptionKeysIo IoOptionsKeys |
  WithOptionKeysWithOptionsTokens |
  WithOptionKeysWithOptionsIndexer
  deriving (Eq, Ord, Read, Show)
_WithOptionKeys = Core.Name "hydra.tinkerpop.gremlin.WithOptionKeys"
_WithOptionKeys_shortestPath = Core.Name "shortestPath"
_WithOptionKeys_connectedComponent = Core.Name "connectedComponent"
_WithOptionKeys_pageRank = Core.Name "pageRank"
_WithOptionKeys_peerPressure = Core.Name "peerPressure"
_WithOptionKeys_io = Core.Name "io"
_WithOptionKeys_withOptionsTokens = Core.Name "withOptionsTokens"
_WithOptionKeys_withOptionsIndexer = Core.Name "withOptionsIndexer"
data WithOptionsValues =
  WithOptionsValuesTokens |
  WithOptionsValuesNone |
  WithOptionsValuesIds |
  WithOptionsValuesLabels |
  WithOptionsValuesKeys |
  WithOptionsValuesValues |
  WithOptionsValuesAll |
  WithOptionsValuesList |
  WithOptionsValuesMap
  deriving (Eq, Ord, Read, Show)
_WithOptionsValues = Core.Name "hydra.tinkerpop.gremlin.WithOptionsValues"
_WithOptionsValues_tokens = Core.Name "tokens"
_WithOptionsValues_none = Core.Name "none"
_WithOptionsValues_ids = Core.Name "ids"
_WithOptionsValues_labels = Core.Name "labels"
_WithOptionsValues_keys = Core.Name "keys"
_WithOptionsValues_values = Core.Name "values"
_WithOptionsValues_all = Core.Name "all"
_WithOptionsValues_list = Core.Name "list"
_WithOptionsValues_map = Core.Name "map"
data WithSackArgs =
  WithSackArgs {
    withSackArgsInitialValue :: GenericLiteralArgument,
    withSackArgsBiFunction :: (Maybe TraversalBiFunctionArgument)}
  deriving (Eq, Ord, Read, Show)
_WithSackArgs = Core.Name "hydra.tinkerpop.gremlin.WithSackArgs"
_WithSackArgs_initialValue = Core.Name "initialValue"
_WithSackArgs_biFunction = Core.Name "biFunction"
