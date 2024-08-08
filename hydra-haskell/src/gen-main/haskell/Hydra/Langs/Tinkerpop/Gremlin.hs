-- | A Gremlin model, based on the Gremlin ANTLR grammar (master branch, as of 2024-06-30).

module Hydra.Langs.Tinkerpop.Gremlin where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype QueryList = 
  QueryList {
    unQueryList :: [Query]}
  deriving (Eq, Ord, Read, Show)

_QueryList = (Core.Name "hydra/langs/tinkerpop/gremlin.QueryList")

_QueryList_type_ = (Core.TypeList _Query_type_)

data Query = 
  QueryTraversalSource TraversalSourceQuery |
  QueryRootTraversal RootTraversalQuery |
  QueryToString  |
  QueryEmpty 
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra/langs/tinkerpop/gremlin.Query")

_Query_traversalSource = (Core.Name "traversalSource")

_Query_rootTraversal = (Core.Name "rootTraversal")

_Query_toString = (Core.Name "toString")

_Query_empty = (Core.Name "empty")

_Query_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.Query"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalSource"),
      Core.fieldTypeType = _TraversalSourceQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rootTraversal"),
      Core.fieldTypeType = _RootTraversalQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toString"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "empty"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalSourceQuery = 
  TraversalSourceQuery {
    traversalSourceQuerySource :: TraversalSource,
    traversalSourceQueryTransactionPart :: (Maybe TransactionPart)}
  deriving (Eq, Ord, Read, Show)

_TraversalSourceQuery = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSourceQuery")

_TraversalSourceQuery_source = (Core.Name "source")

_TraversalSourceQuery_transactionPart = (Core.Name "transactionPart")

_TraversalSourceQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSourceQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "source"),
      Core.fieldTypeType = _TraversalSource_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "transactionPart"),
      Core.fieldTypeType = (Core.TypeOptional _TransactionPart_type_)}]}))

data RootTraversalQuery = 
  RootTraversalQuery {
    rootTraversalQueryRoot :: RootTraversal,
    rootTraversalQueryTerminalMethod :: (Maybe TraversalTerminalMethod)}
  deriving (Eq, Ord, Read, Show)

_RootTraversalQuery = (Core.Name "hydra/langs/tinkerpop/gremlin.RootTraversalQuery")

_RootTraversalQuery_root = (Core.Name "root")

_RootTraversalQuery_terminalMethod = (Core.Name "terminalMethod")

_RootTraversalQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.RootTraversalQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "root"),
      Core.fieldTypeType = _RootTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "terminalMethod"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalTerminalMethod_type_)}]}))

newtype TraversalSource = 
  TraversalSource {
    unTraversalSource :: [TraversalSourceSelfMethod]}
  deriving (Eq, Ord, Read, Show)

_TraversalSource = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSource")

_TraversalSource_type_ = (Core.TypeList _TraversalSourceSelfMethod_type_)

data TransactionPart = 
  TransactionPartBegin  |
  TransactionPartCommit  |
  TransactionPartRollback 
  deriving (Eq, Ord, Read, Show)

_TransactionPart = (Core.Name "hydra/langs/tinkerpop/gremlin.TransactionPart")

_TransactionPart_begin = (Core.Name "begin")

_TransactionPart_commit = (Core.Name "commit")

_TransactionPart_rollback = (Core.Name "rollback")

_TransactionPart_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TransactionPart"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "begin"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "commit"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rollback"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data RootTraversal = 
  RootTraversal {
    rootTraversalSource :: TraversalSource,
    rootTraversalSpawnMethod :: TraversalSourceSpawnMethod,
    rootTraversalChained :: [ChainedTraversalElement]}
  deriving (Eq, Ord, Read, Show)

_RootTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.RootTraversal")

_RootTraversal_source = (Core.Name "source")

_RootTraversal_spawnMethod = (Core.Name "spawnMethod")

_RootTraversal_chained = (Core.Name "chained")

_RootTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.RootTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "source"),
      Core.fieldTypeType = _TraversalSource_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "spawnMethod"),
      Core.fieldTypeType = _TraversalSourceSpawnMethod_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "chained"),
      Core.fieldTypeType = (Core.TypeList _ChainedTraversalElement_type_)}]}))

data TraversalSourceSelfMethod = 
  TraversalSourceSelfMethodWithBulk Bool |
  TraversalSourceSelfMethodWithPath  |
  TraversalSourceSelfMethodWithSack GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument |
  TraversalSourceSelfMethodWithSideEffect StringArgumentAndGenericLiteralArgument |
  TraversalSourceSelfMethodWithStrategies [TraversalStrategy] |
  TraversalSourceSelfMethodWithoutStrategies [Identifier] |
  TraversalSourceSelfMethodWith StringArgumentAndOptionalGenericLiteralArgument
  deriving (Eq, Ord, Read, Show)

_TraversalSourceSelfMethod = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSourceSelfMethod")

_TraversalSourceSelfMethod_withBulk = (Core.Name "withBulk")

_TraversalSourceSelfMethod_withPath = (Core.Name "withPath")

_TraversalSourceSelfMethod_withSack = (Core.Name "withSack")

_TraversalSourceSelfMethod_withSideEffect = (Core.Name "withSideEffect")

_TraversalSourceSelfMethod_withStrategies = (Core.Name "withStrategies")

_TraversalSourceSelfMethod_withoutStrategies = (Core.Name "withoutStrategies")

_TraversalSourceSelfMethod_with = (Core.Name "with")

_TraversalSourceSelfMethod_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSourceSelfMethod"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withBulk"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withPath"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withSack"),
      Core.fieldTypeType = _GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withSideEffect"),
      Core.fieldTypeType = _StringArgumentAndGenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withStrategies"),
      Core.fieldTypeType = (Core.TypeList _TraversalStrategy_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withoutStrategies"),
      Core.fieldTypeType = (Core.TypeList _Identifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "with"),
      Core.fieldTypeType = _StringArgumentAndOptionalGenericLiteralArgument_type_}]}))

data GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument = 
  GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument {
    genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentLiteral :: GenericLiteralArgument,
    genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentBiFunction :: (Maybe TraversalBiFunctionArgument)}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument")

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_literal = (Core.Name "literal")

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_biFunction = (Core.Name "biFunction")

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "biFunction"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalBiFunctionArgument_type_)}]}))

data StringArgumentAndGenericLiteralArgument = 
  StringArgumentAndGenericLiteralArgument {
    stringArgumentAndGenericLiteralArgumentString :: StringArgument,
    stringArgumentAndGenericLiteralArgumentLiteral :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndGenericLiteralArgument")

_StringArgumentAndGenericLiteralArgument_string = (Core.Name "string")

_StringArgumentAndGenericLiteralArgument_literal = (Core.Name "literal")

_StringArgumentAndGenericLiteralArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndGenericLiteralArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _GenericLiteralArgument_type_}]}))

data StringArgumentAndOptionalGenericLiteralArgument = 
  StringArgumentAndOptionalGenericLiteralArgument {
    stringArgumentAndOptionalGenericLiteralArgumentString :: StringArgument,
    stringArgumentAndOptionalGenericLiteralArgumentLiteral :: (Maybe GenericLiteralArgument)}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndOptionalGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndOptionalGenericLiteralArgument")

_StringArgumentAndOptionalGenericLiteralArgument_string = (Core.Name "string")

_StringArgumentAndOptionalGenericLiteralArgument_literal = (Core.Name "literal")

_StringArgumentAndOptionalGenericLiteralArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndOptionalGenericLiteralArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralArgument_type_)}]}))

data TraversalSourceSpawnMethod = 
  TraversalSourceSpawnMethodAddE StringArgumentOrNestedTraversal |
  TraversalSourceSpawnMethodAddV (Maybe StringArgumentOrNestedTraversal) |
  TraversalSourceSpawnMethodE [GenericLiteralArgument] |
  TraversalSourceSpawnMethodV [GenericLiteralArgument] |
  TraversalSourceSpawnMethodMergeV GenericLiteralMapNullableArgumentOrNestedTraversal |
  TraversalSourceSpawnMethodMergeE GenericLiteralMapNullableArgumentOrNestedTraversal |
  TraversalSourceSpawnMethodInject [GenericLiteralArgument] |
  TraversalSourceSpawnMethodIo StringArgument |
  TraversalSourceSpawnMethodCall (Maybe ServiceCall) |
  TraversalSourceSpawnMethodUnion [NestedTraversal]
  deriving (Eq, Ord, Read, Show)

_TraversalSourceSpawnMethod = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSourceSpawnMethod")

_TraversalSourceSpawnMethod_addE = (Core.Name "addE")

_TraversalSourceSpawnMethod_addV = (Core.Name "addV")

_TraversalSourceSpawnMethod_e = (Core.Name "e")

_TraversalSourceSpawnMethod_v = (Core.Name "v")

_TraversalSourceSpawnMethod_mergeV = (Core.Name "mergeV")

_TraversalSourceSpawnMethod_mergeE = (Core.Name "mergeE")

_TraversalSourceSpawnMethod_inject = (Core.Name "inject")

_TraversalSourceSpawnMethod_io = (Core.Name "io")

_TraversalSourceSpawnMethod_call = (Core.Name "call")

_TraversalSourceSpawnMethod_union = (Core.Name "union")

_TraversalSourceSpawnMethod_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSourceSpawnMethod"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "addE"),
      Core.fieldTypeType = _StringArgumentOrNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "addV"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgumentOrNestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "e"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "v"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mergeV"),
      Core.fieldTypeType = _GenericLiteralMapNullableArgumentOrNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mergeE"),
      Core.fieldTypeType = _GenericLiteralMapNullableArgumentOrNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inject"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "io"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "call"),
      Core.fieldTypeType = (Core.TypeOptional _ServiceCall_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "union"),
      Core.fieldTypeType = (Core.TypeList _NestedTraversal_type_)}]}))

data GenericLiteralMapNullableArgumentOrNestedTraversal = 
  GenericLiteralMapNullableArgumentOrNestedTraversalMap GenericLiteralMapNullableArgument |
  GenericLiteralMapNullableArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgumentOrNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal")

_GenericLiteralMapNullableArgumentOrNestedTraversal_map = (Core.Name "map")

_GenericLiteralMapNullableArgumentOrNestedTraversal_traversal = (Core.Name "traversal")

_GenericLiteralMapNullableArgumentOrNestedTraversal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _GenericLiteralMapNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data ServiceCall = 
  ServiceCall {
    serviceCallService :: StringArgument,
    serviceCallArguments :: ServiceArguments}
  deriving (Eq, Ord, Read, Show)

_ServiceCall = (Core.Name "hydra/langs/tinkerpop/gremlin.ServiceCall")

_ServiceCall_service = (Core.Name "service")

_ServiceCall_arguments = (Core.Name "arguments")

_ServiceCall_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ServiceCall"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "service"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = _ServiceArguments_type_}]}))

data ServiceArguments = 
  ServiceArgumentsMap (Maybe GenericLiteralMapArgument) |
  ServiceArgumentsTraversal (Maybe NestedTraversal)
  deriving (Eq, Ord, Read, Show)

_ServiceArguments = (Core.Name "hydra/langs/tinkerpop/gremlin.ServiceArguments")

_ServiceArguments_map = (Core.Name "map")

_ServiceArguments_traversal = (Core.Name "traversal")

_ServiceArguments_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ServiceArguments"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralMapArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = (Core.TypeOptional _NestedTraversal_type_)}]}))

data ChainedTraversal = 
  ChainedTraversal {
    chainedTraversalFirst :: TraversalMethod,
    chainedTraversalRest :: ChainedTraversalElement}
  deriving (Eq, Ord, Read, Show)

_ChainedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.ChainedTraversal")

_ChainedTraversal_first = (Core.Name "first")

_ChainedTraversal_rest = (Core.Name "rest")

_ChainedTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ChainedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = _TraversalMethod_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = _ChainedTraversalElement_type_}]}))

data ChainedTraversalElement = 
  ChainedTraversalElementMethod TraversalMethod |
  ChainedTraversalElementSelf TraversalSelfMethod
  deriving (Eq, Ord, Read, Show)

_ChainedTraversalElement = (Core.Name "hydra/langs/tinkerpop/gremlin.ChainedTraversalElement")

_ChainedTraversalElement_method = (Core.Name "method")

_ChainedTraversalElement_self = (Core.Name "self")

_ChainedTraversalElement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ChainedTraversalElement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "method"),
      Core.fieldTypeType = _TraversalMethod_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "self"),
      Core.fieldTypeType = _TraversalSelfMethod_type_}]}))

data NestedTraversal = 
  NestedTraversalRoot RootTraversal |
  NestedTraversalChained ChainedTraversal |
  NestedTraversalAnonymous ChainedTraversal
  deriving (Eq, Ord, Read, Show)

_NestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.NestedTraversal")

_NestedTraversal_root = (Core.Name "root")

_NestedTraversal_chained = (Core.Name "chained")

_NestedTraversal_anonymous = (Core.Name "anonymous")

_NestedTraversal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.NestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "root"),
      Core.fieldTypeType = _RootTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "chained"),
      Core.fieldTypeType = _ChainedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "anonymous"),
      Core.fieldTypeType = _ChainedTraversal_type_}]}))

data TerminatedTraversal = 
  TerminatedTraversal {
    terminatedTraversalRoot :: RootTraversal,
    terminatedTraversalTerminal :: TraversalTerminalMethod}
  deriving (Eq, Ord, Read, Show)

_TerminatedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TerminatedTraversal")

_TerminatedTraversal_root = (Core.Name "root")

_TerminatedTraversal_terminal = (Core.Name "terminal")

_TerminatedTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TerminatedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "root"),
      Core.fieldTypeType = _RootTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "terminal"),
      Core.fieldTypeType = _TraversalTerminalMethod_type_}]}))

data TraversalMethod = 
  TraversalMethodV [GenericLiteralArgument] |
  TraversalMethodE [GenericLiteralArgument] |
  TraversalMethodAddE StringArgumentOrNestedTraversal |
  TraversalMethodAddV (Maybe StringArgumentOrNestedTraversal) |
  TraversalMethodMergeE (Maybe GenericLiteralMapNullableArgumentOrNestedTraversal) |
  TraversalMethodMergeV (Maybe GenericLiteralMapNullableArgumentOrNestedTraversal) |
  TraversalMethodAggregate OptionalTraversalScopeArgumentAndStringArgument |
  TraversalMethodAll TraversalPredicate |
  TraversalMethodAnd [NestedTraversal] |
  TraversalMethodAny TraversalPredicate |
  TraversalMethodAs StringArgumentAndOptionalStringLiteralVarargs |
  TraversalMethodBarrier (Maybe TraversalSackMethodArgumentOrIntegerArgument) |
  TraversalMethodBoth [StringNullableArgument] |
  TraversalMethodBothE [StringNullableArgument] |
  TraversalMethodBothV  |
  TraversalMethodBranch NestedTraversal |
  TraversalMethodBy ByArgs |
  TraversalMethodCap StringArgumentAndOptionalStringLiteralVarargs |
  TraversalMethodChoose ChooseArgs |
  TraversalMethodCoalesce [NestedTraversal] |
  TraversalMethodCoin FloatArgument |
  TraversalMethodConjoin StringArgument |
  TraversalMethodConnectedComponent  |
  TraversalMethodConstant GenericLiteralArgument |
  TraversalMethodCount (Maybe TraversalScopeArgument) |
  TraversalMethodCyclicPath  |
  TraversalMethodDedup DedupArgs |
  TraversalMethodDifference GenericLiteralArgument |
  TraversalMethodDisjunct GenericLiteralArgument |
  TraversalMethodDrop  |
  TraversalMethodElementMap [StringNullableArgument] |
  TraversalMethodEmit (Maybe PredicateOrTraversal) |
  TraversalMethodFilter PredicateOrTraversal |
  TraversalMethodFlatMap NestedTraversal |
  TraversalMethodFold (Maybe GenericLiteralArgumentAndTraversalBiFunctionArgument) |
  TraversalMethodFrom FromArgs |
  TraversalMethodGroup (Maybe StringArgument) |
  TraversalMethodGroupCount (Maybe StringArgument) |
  TraversalMethodHas HasArgs |
  TraversalMethodHasId GenericLiteralArgumentAndTraversalPredicate |
  TraversalMethodHasKey TraversalPredicateOrStringLiteralVarargs |
  TraversalMethodHasLabel TraversalPredicateOrStringLiteralVarargs |
  TraversalMethodHasNot StringNullableArgument |
  TraversalMethodHasValue TraversalPredicateOrGenericLiteralArgument |
  TraversalMethodId  |
  TraversalMethodIdentity  |
  TraversalMethodIn [StringNullableArgument] |
  TraversalMethodInE [StringNullableArgument] |
  TraversalMethodIntersect GenericLiteralArgument |
  TraversalMethodInV  |
  TraversalMethodIndex  |
  TraversalMethodInject [GenericLiteralArgument] |
  TraversalMethodIs TraversalPredicateOrGenericLiteralArgument |
  TraversalMethodKey  |
  TraversalMethodLabel  |
  TraversalMethodLimit OptionalTraversalScopeArgumentAndIntegerArgument |
  TraversalMethodLocal NestedTraversal |
  TraversalMethodLoops (Maybe StringArgument) |
  TraversalMethodMap NestedTraversal |
  TraversalMethodMatch [NestedTraversal] |
  TraversalMethodMath StringArgument |
  TraversalMethodMax (Maybe TraversalScopeArgument) |
  TraversalMethodMean (Maybe TraversalScopeArgument) |
  TraversalMethodMin (Maybe TraversalScopeArgument) |
  TraversalMethodNone TraversalPredicate |
  TraversalMethodNot NestedTraversal |
  TraversalMethodOption OptionArgs |
  TraversalMethodOptional NestedTraversal |
  TraversalMethodOr [NestedTraversal] |
  TraversalMethodOrder (Maybe TraversalScopeArgument) |
  TraversalMethodOtherV  |
  TraversalMethodOut [StringNullableArgument] |
  TraversalMethodOutE [StringNullableArgument] |
  TraversalMethodOutV  |
  TraversalMethodPageRank (Maybe FloatArgument) |
  TraversalMethodPath  |
  TraversalMethodPeerPressure  |
  TraversalMethodProfile (Maybe StringArgument) |
  TraversalMethodProject StringArgumentAndOptionalStringLiteralVarargs |
  TraversalMethodProperties [StringNullableArgument] |
  TraversalMethodProperty PropertyArgs |
  TraversalMethodPropertyMap [StringNullableArgument] |
  TraversalMethodRange RangeArgs |
  TraversalMethodRead  |
  TraversalMethodRepeat OptionalStringArgumentAndNestedTraversal |
  TraversalMethodSack (Maybe TraversalBiFunctionArgument) |
  TraversalMethodSample OptionalTraversalScopeArgumentAndIntegerArgument |
  TraversalMethodSelect SelectArgs |
  TraversalMethodCombine GenericLiteralArgument |
  TraversalMethodProduct GenericLiteralArgument |
  TraversalMethodMerge GenericLiteralArgument |
  TraversalMethodShortestPath  |
  TraversalMethodSideEffect NestedTraversal |
  TraversalMethodSimplePath  |
  TraversalMethodSkip OptionalTraversalScopeArgumentAndIntegerArgument |
  TraversalMethodStore StringArgument |
  TraversalMethodSubgraph StringArgument |
  TraversalMethodSum (Maybe TraversalScopeArgument) |
  TraversalMethodTail (Maybe TailArgs) |
  TraversalMethodFail (Maybe StringArgument) |
  TraversalMethodTimes IntegerArgument |
  TraversalMethodTo ToArgs |
  TraversalMethodToE DirectionAndVarargs |
  TraversalMethodToV TraversalDirectionArgument |
  TraversalMethodTree (Maybe StringArgument) |
  TraversalMethodUnfold  |
  TraversalMethodUnion [NestedTraversal] |
  TraversalMethodUntil PredicateOrTraversal |
  TraversalMethodValue  |
  TraversalMethodValueMap ValueMapArgs |
  TraversalMethodValues [StringNullableArgument] |
  TraversalMethodWhere WhereArgs |
  TraversalMethodWith WithArgs |
  TraversalMethodWrite  |
  TraversalMethodElement [StringNullableArgument] |
  TraversalMethodCall ServiceCall |
  TraversalMethodConcat ConcatArgs |
  TraversalMethodAsString (Maybe TraversalScopeArgument) |
  TraversalMethodFormat StringArgument |
  TraversalMethodToUpper (Maybe TraversalScopeArgument) |
  TraversalMethodToLower (Maybe TraversalScopeArgument) |
  TraversalMethodLength (Maybe TraversalScopeArgument) |
  TraversalMethodTrim (Maybe TraversalScopeArgument) |
  TraversalMethodLTrim (Maybe TraversalScopeArgument) |
  TraversalMethodRTrim (Maybe TraversalScopeArgument) |
  TraversalMethodReverse  |
  TraversalMethodReplace ReplaceArgs |
  TraversalMethodSplit SplitArgs |
  TraversalMethodSubstring SubstringArgs |
  TraversalMethodAsDate  |
  TraversalMethodDateAdd DateAddArgs |
  TraversalMethodDateDiff DateDiffArgs
  deriving (Eq, Ord, Read, Show)

_TraversalMethod = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMethod")

_TraversalMethod_v = (Core.Name "v")

_TraversalMethod_e = (Core.Name "e")

_TraversalMethod_addE = (Core.Name "addE")

_TraversalMethod_addV = (Core.Name "addV")

_TraversalMethod_mergeE = (Core.Name "mergeE")

_TraversalMethod_mergeV = (Core.Name "mergeV")

_TraversalMethod_aggregate = (Core.Name "aggregate")

_TraversalMethod_all = (Core.Name "all")

_TraversalMethod_and = (Core.Name "and")

_TraversalMethod_any = (Core.Name "any")

_TraversalMethod_as = (Core.Name "as")

_TraversalMethod_barrier = (Core.Name "barrier")

_TraversalMethod_both = (Core.Name "both")

_TraversalMethod_bothE = (Core.Name "bothE")

_TraversalMethod_bothV = (Core.Name "bothV")

_TraversalMethod_branch = (Core.Name "branch")

_TraversalMethod_by = (Core.Name "by")

_TraversalMethod_cap = (Core.Name "cap")

_TraversalMethod_choose = (Core.Name "choose")

_TraversalMethod_coalesce = (Core.Name "coalesce")

_TraversalMethod_coin = (Core.Name "coin")

_TraversalMethod_conjoin = (Core.Name "conjoin")

_TraversalMethod_connectedComponent = (Core.Name "connectedComponent")

_TraversalMethod_constant = (Core.Name "constant")

_TraversalMethod_count = (Core.Name "count")

_TraversalMethod_cyclicPath = (Core.Name "cyclicPath")

_TraversalMethod_dedup = (Core.Name "dedup")

_TraversalMethod_difference = (Core.Name "difference")

_TraversalMethod_disjunct = (Core.Name "disjunct")

_TraversalMethod_drop = (Core.Name "drop")

_TraversalMethod_elementMap = (Core.Name "elementMap")

_TraversalMethod_emit = (Core.Name "emit")

_TraversalMethod_filter = (Core.Name "filter")

_TraversalMethod_flatMap = (Core.Name "flatMap")

_TraversalMethod_fold = (Core.Name "fold")

_TraversalMethod_from = (Core.Name "from")

_TraversalMethod_group = (Core.Name "group")

_TraversalMethod_groupCount = (Core.Name "groupCount")

_TraversalMethod_has = (Core.Name "has")

_TraversalMethod_hasId = (Core.Name "hasId")

_TraversalMethod_hasKey = (Core.Name "hasKey")

_TraversalMethod_hasLabel = (Core.Name "hasLabel")

_TraversalMethod_hasNot = (Core.Name "hasNot")

_TraversalMethod_hasValue = (Core.Name "hasValue")

_TraversalMethod_id = (Core.Name "id")

_TraversalMethod_identity = (Core.Name "identity")

_TraversalMethod_in = (Core.Name "in")

_TraversalMethod_inE = (Core.Name "inE")

_TraversalMethod_intersect = (Core.Name "intersect")

_TraversalMethod_inV = (Core.Name "inV")

_TraversalMethod_index = (Core.Name "index")

_TraversalMethod_inject = (Core.Name "inject")

_TraversalMethod_is = (Core.Name "is")

_TraversalMethod_key = (Core.Name "key")

_TraversalMethod_label = (Core.Name "label")

_TraversalMethod_limit = (Core.Name "limit")

_TraversalMethod_local = (Core.Name "local")

_TraversalMethod_loops = (Core.Name "loops")

_TraversalMethod_map = (Core.Name "map")

_TraversalMethod_match = (Core.Name "match")

_TraversalMethod_math = (Core.Name "math")

_TraversalMethod_max = (Core.Name "max")

_TraversalMethod_mean = (Core.Name "mean")

_TraversalMethod_min = (Core.Name "min")

_TraversalMethod_none = (Core.Name "none")

_TraversalMethod_not = (Core.Name "not")

_TraversalMethod_option = (Core.Name "option")

_TraversalMethod_optional = (Core.Name "optional")

_TraversalMethod_or = (Core.Name "or")

_TraversalMethod_order = (Core.Name "order")

_TraversalMethod_otherV = (Core.Name "otherV")

_TraversalMethod_out = (Core.Name "out")

_TraversalMethod_outE = (Core.Name "outE")

_TraversalMethod_outV = (Core.Name "outV")

_TraversalMethod_pageRank = (Core.Name "pageRank")

_TraversalMethod_path = (Core.Name "path")

_TraversalMethod_peerPressure = (Core.Name "peerPressure")

_TraversalMethod_profile = (Core.Name "profile")

_TraversalMethod_project = (Core.Name "project")

_TraversalMethod_properties = (Core.Name "properties")

_TraversalMethod_property = (Core.Name "property")

_TraversalMethod_propertyMap = (Core.Name "propertyMap")

_TraversalMethod_range = (Core.Name "range")

_TraversalMethod_read = (Core.Name "read")

_TraversalMethod_repeat = (Core.Name "repeat")

_TraversalMethod_sack = (Core.Name "sack")

_TraversalMethod_sample = (Core.Name "sample")

_TraversalMethod_select = (Core.Name "select")

_TraversalMethod_combine = (Core.Name "combine")

_TraversalMethod_product = (Core.Name "product")

_TraversalMethod_merge = (Core.Name "merge")

_TraversalMethod_shortestPath = (Core.Name "shortestPath")

_TraversalMethod_sideEffect = (Core.Name "sideEffect")

_TraversalMethod_simplePath = (Core.Name "simplePath")

_TraversalMethod_skip = (Core.Name "skip")

_TraversalMethod_store = (Core.Name "store")

_TraversalMethod_subgraph = (Core.Name "subgraph")

_TraversalMethod_sum = (Core.Name "sum")

_TraversalMethod_tail = (Core.Name "tail")

_TraversalMethod_fail = (Core.Name "fail")

_TraversalMethod_times = (Core.Name "times")

_TraversalMethod_to = (Core.Name "to")

_TraversalMethod_toE = (Core.Name "toE")

_TraversalMethod_toV = (Core.Name "toV")

_TraversalMethod_tree = (Core.Name "tree")

_TraversalMethod_unfold = (Core.Name "unfold")

_TraversalMethod_union = (Core.Name "union")

_TraversalMethod_until = (Core.Name "until")

_TraversalMethod_value = (Core.Name "value")

_TraversalMethod_valueMap = (Core.Name "valueMap")

_TraversalMethod_values = (Core.Name "values")

_TraversalMethod_where = (Core.Name "where")

_TraversalMethod_with = (Core.Name "with")

_TraversalMethod_write = (Core.Name "write")

_TraversalMethod_element = (Core.Name "element")

_TraversalMethod_call = (Core.Name "call")

_TraversalMethod_concat = (Core.Name "concat")

_TraversalMethod_asString = (Core.Name "asString")

_TraversalMethod_format = (Core.Name "format")

_TraversalMethod_toUpper = (Core.Name "toUpper")

_TraversalMethod_toLower = (Core.Name "toLower")

_TraversalMethod_length = (Core.Name "length")

_TraversalMethod_trim = (Core.Name "trim")

_TraversalMethod_lTrim = (Core.Name "lTrim")

_TraversalMethod_rTrim = (Core.Name "rTrim")

_TraversalMethod_reverse = (Core.Name "reverse")

_TraversalMethod_replace = (Core.Name "replace")

_TraversalMethod_split = (Core.Name "split")

_TraversalMethod_substring = (Core.Name "substring")

_TraversalMethod_asDate = (Core.Name "asDate")

_TraversalMethod_dateAdd = (Core.Name "dateAdd")

_TraversalMethod_dateDiff = (Core.Name "dateDiff")

_TraversalMethod_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMethod"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "v"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "e"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "addE"),
      Core.fieldTypeType = _StringArgumentOrNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "addV"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgumentOrNestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mergeE"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralMapNullableArgumentOrNestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mergeV"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralMapNullableArgumentOrNestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "aggregate"),
      Core.fieldTypeType = _OptionalTraversalScopeArgumentAndStringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = (Core.TypeList _NestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "any"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "as"),
      Core.fieldTypeType = _StringArgumentAndOptionalStringLiteralVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "barrier"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalSackMethodArgumentOrIntegerArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "both"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bothE"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bothV"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "branch"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "by"),
      Core.fieldTypeType = _ByArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cap"),
      Core.fieldTypeType = _StringArgumentAndOptionalStringLiteralVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "choose"),
      Core.fieldTypeType = _ChooseArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "coalesce"),
      Core.fieldTypeType = (Core.TypeList _NestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "coin"),
      Core.fieldTypeType = _FloatArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "conjoin"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "connectedComponent"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constant"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "count"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cyclicPath"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dedup"),
      Core.fieldTypeType = _DedupArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "difference"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "disjunct"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "drop"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "elementMap"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "emit"),
      Core.fieldTypeType = (Core.TypeOptional _PredicateOrTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "filter"),
      Core.fieldTypeType = _PredicateOrTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "flatMap"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fold"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralArgumentAndTraversalBiFunctionArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "from"),
      Core.fieldTypeType = _FromArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "group"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "groupCount"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "has"),
      Core.fieldTypeType = _HasArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasId"),
      Core.fieldTypeType = _GenericLiteralArgumentAndTraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasKey"),
      Core.fieldTypeType = _TraversalPredicateOrStringLiteralVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasLabel"),
      Core.fieldTypeType = _TraversalPredicateOrStringLiteralVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasNot"),
      Core.fieldTypeType = _StringNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasValue"),
      Core.fieldTypeType = _TraversalPredicateOrGenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identity"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "in"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inE"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "intersect"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inV"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "index"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inject"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "is"),
      Core.fieldTypeType = _TraversalPredicateOrGenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "limit"),
      Core.fieldTypeType = _OptionalTraversalScopeArgumentAndIntegerArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "local"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "loops"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "match"),
      Core.fieldTypeType = (Core.TypeList _NestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "math"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "max"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mean"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "min"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "not"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "option"),
      Core.fieldTypeType = _OptionArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "optional"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeList _NestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "order"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "otherV"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "out"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outE"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outV"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pageRank"),
      Core.fieldTypeType = (Core.TypeOptional _FloatArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "path"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "peerPressure"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "profile"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "project"),
      Core.fieldTypeType = _StringArgumentAndOptionalStringLiteralVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _PropertyArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "propertyMap"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = _RangeArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "read"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "repeat"),
      Core.fieldTypeType = _OptionalStringArgumentAndNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sack"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalBiFunctionArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sample"),
      Core.fieldTypeType = _OptionalTraversalScopeArgumentAndIntegerArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "select"),
      Core.fieldTypeType = _SelectArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "combine"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "product"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "merge"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shortestPath"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sideEffect"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simplePath"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "skip"),
      Core.fieldTypeType = _OptionalTraversalScopeArgumentAndIntegerArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "store"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subgraph"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sum"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tail"),
      Core.fieldTypeType = (Core.TypeOptional _TailArgs_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fail"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "times"),
      Core.fieldTypeType = _IntegerArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "to"),
      Core.fieldTypeType = _ToArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toE"),
      Core.fieldTypeType = _DirectionAndVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toV"),
      Core.fieldTypeType = _TraversalDirectionArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tree"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unfold"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "union"),
      Core.fieldTypeType = (Core.TypeList _NestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "until"),
      Core.fieldTypeType = _PredicateOrTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "valueMap"),
      Core.fieldTypeType = _ValueMapArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "values"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = _WhereArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "with"),
      Core.fieldTypeType = _WithArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "write"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "element"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "call"),
      Core.fieldTypeType = _ServiceCall_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "concat"),
      Core.fieldTypeType = _ConcatArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "asString"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "format"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toUpper"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toLower"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "length"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "trim"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lTrim"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rTrim"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reverse"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "replace"),
      Core.fieldTypeType = _ReplaceArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "split"),
      Core.fieldTypeType = _SplitArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "substring"),
      Core.fieldTypeType = _SubstringArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "asDate"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dateAdd"),
      Core.fieldTypeType = _DateAddArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dateDiff"),
      Core.fieldTypeType = _DateDiffArgs_type_}]}))

data StringArgumentOrNestedTraversal = 
  StringArgumentOrNestedTraversalString StringArgument |
  StringArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_StringArgumentOrNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentOrNestedTraversal")

_StringArgumentOrNestedTraversal_string = (Core.Name "string")

_StringArgumentOrNestedTraversal_traversal = (Core.Name "traversal")

_StringArgumentOrNestedTraversal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentOrNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data OptionalTraversalScopeArgumentAndStringArgument = 
  OptionalTraversalScopeArgumentAndStringArgument {
    optionalTraversalScopeArgumentAndStringArgumentScope :: (Maybe TraversalScopeArgument),
    optionalTraversalScopeArgumentAndStringArgumentString :: StringArgument}
  deriving (Eq, Ord, Read, Show)

_OptionalTraversalScopeArgumentAndStringArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndStringArgument")

_OptionalTraversalScopeArgumentAndStringArgument_scope = (Core.Name "scope")

_OptionalTraversalScopeArgumentAndStringArgument_string = (Core.Name "string")

_OptionalTraversalScopeArgumentAndStringArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndStringArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_}]}))

data StringArgumentAndOptionalStringLiteralVarargs = 
  StringArgumentAndOptionalStringLiteralVarargs {
    stringArgumentAndOptionalStringLiteralVarargsFirst :: StringArgument,
    stringArgumentAndOptionalStringLiteralVarargsRest :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndOptionalStringLiteralVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndOptionalStringLiteralVarargs")

_StringArgumentAndOptionalStringLiteralVarargs_first = (Core.Name "first")

_StringArgumentAndOptionalStringLiteralVarargs_rest = (Core.Name "rest")

_StringArgumentAndOptionalStringLiteralVarargs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndOptionalStringLiteralVarargs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)}]}))

data TraversalSackMethodArgumentOrIntegerArgument = 
  TraversalSackMethodArgumentOrIntegerArgumentConsumer TraversalSackMethodArgument |
  TraversalSackMethodArgumentOrIntegerArgumentInt IntegerArgument
  deriving (Eq, Ord, Read, Show)

_TraversalSackMethodArgumentOrIntegerArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSackMethodArgumentOrIntegerArgument")

_TraversalSackMethodArgumentOrIntegerArgument_consumer = (Core.Name "consumer")

_TraversalSackMethodArgumentOrIntegerArgument_int = (Core.Name "int")

_TraversalSackMethodArgumentOrIntegerArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSackMethodArgumentOrIntegerArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "consumer"),
      Core.fieldTypeType = _TraversalSackMethodArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = _IntegerArgument_type_}]}))

data ByArgs = 
  ByArgsOrder TraversalOrderArgument |
  ByArgsToken TraversalTokenArgument |
  ByArgsOther ByOtherArgs
  deriving (Eq, Ord, Read, Show)

_ByArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ByArgs")

_ByArgs_order = (Core.Name "order")

_ByArgs_token = (Core.Name "token")

_ByArgs_other = (Core.Name "other")

_ByArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ByArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "order"),
      Core.fieldTypeType = _TraversalOrderArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "token"),
      Core.fieldTypeType = _TraversalTokenArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "other"),
      Core.fieldTypeType = _ByOtherArgs_type_}]}))

data ByOtherArgs = 
  ByOtherArgsComparator (Maybe TraversalComparatorArgument) |
  ByOtherArgsOther (Maybe TraversalFunctionArgumentOrStringArgumentOrNestedTraversal)
  deriving (Eq, Ord, Read, Show)

_ByOtherArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ByOtherArgs")

_ByOtherArgs_comparator = (Core.Name "comparator")

_ByOtherArgs_other = (Core.Name "other")

_ByOtherArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ByOtherArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comparator"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalComparatorArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "other"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_type_)}]}))

data TraversalFunctionArgumentOrStringArgumentOrNestedTraversal = 
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction TraversalFunctionArgument |
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalString StringArgument |
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_function = (Core.Name "function")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_string = (Core.Name "string")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_traversal = (Core.Name "traversal")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "function"),
      Core.fieldTypeType = _TraversalFunctionArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data ChooseArgs = 
  ChooseArgsFunction TraversalFunctionArgument |
  ChooseArgsPredicateTraversal PredicateTraversalArgument |
  ChooseArgsTraversal NestedTraversalArgument
  deriving (Eq, Ord, Read, Show)

_ChooseArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ChooseArgs")

_ChooseArgs_function = (Core.Name "function")

_ChooseArgs_predicateTraversal = (Core.Name "predicateTraversal")

_ChooseArgs_traversal = (Core.Name "traversal")

_ChooseArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ChooseArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "function"),
      Core.fieldTypeType = _TraversalFunctionArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicateTraversal"),
      Core.fieldTypeType = _PredicateTraversalArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversalArgument_type_}]}))

data PredicateTraversalArgument = 
  PredicateTraversalArgument {
    predicateTraversalArgumentPredicate :: TraversalPredicate,
    predicateTraversalArgumentTraversal1 :: NestedTraversal,
    predicateTraversalArgumentTraversal2 :: (Maybe NestedTraversal)}
  deriving (Eq, Ord, Read, Show)

_PredicateTraversalArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.PredicateTraversalArgument")

_PredicateTraversalArgument_predicate = (Core.Name "predicate")

_PredicateTraversalArgument_traversal1 = (Core.Name "traversal1")

_PredicateTraversalArgument_traversal2 = (Core.Name "traversal2")

_PredicateTraversalArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.PredicateTraversalArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal1"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal2"),
      Core.fieldTypeType = (Core.TypeOptional _NestedTraversal_type_)}]}))

data NestedTraversalArgument = 
  NestedTraversalArgument {
    nestedTraversalArgumentTraversal1 :: NestedTraversal,
    nestedTraversalArgumentTraversal2 :: (Maybe NestedTraversal),
    nestedTraversalArgumentTraversal3 :: (Maybe NestedTraversal)}
  deriving (Eq, Ord, Read, Show)

_NestedTraversalArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.NestedTraversalArgument")

_NestedTraversalArgument_traversal1 = (Core.Name "traversal1")

_NestedTraversalArgument_traversal2 = (Core.Name "traversal2")

_NestedTraversalArgument_traversal3 = (Core.Name "traversal3")

_NestedTraversalArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.NestedTraversalArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal1"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal2"),
      Core.fieldTypeType = (Core.TypeOptional _NestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal3"),
      Core.fieldTypeType = (Core.TypeOptional _NestedTraversal_type_)}]}))

data DedupArgs = 
  DedupArgsScopeString ScopeStringArgument |
  DedupArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_DedupArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.DedupArgs")

_DedupArgs_scopeString = (Core.Name "scopeString")

_DedupArgs_string = (Core.Name "string")

_DedupArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.DedupArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scopeString"),
      Core.fieldTypeType = _ScopeStringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)}]}))

data ScopeStringArgument = 
  ScopeStringArgument {
    scopeStringArgumentScope :: TraversalScopeArgument,
    scopeStringArgumentStrings :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_ScopeStringArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.ScopeStringArgument")

_ScopeStringArgument_scope = (Core.Name "scope")

_ScopeStringArgument_strings = (Core.Name "strings")

_ScopeStringArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ScopeStringArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = _TraversalScopeArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "strings"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)}]}))

data PredicateOrTraversal = 
  PredicateOrTraversalPredicate TraversalPredicate |
  PredicateOrTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_PredicateOrTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.PredicateOrTraversal")

_PredicateOrTraversal_predicate = (Core.Name "predicate")

_PredicateOrTraversal_traversal = (Core.Name "traversal")

_PredicateOrTraversal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.PredicateOrTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data GenericLiteralArgumentAndTraversalBiFunctionArgument = 
  GenericLiteralArgumentAndTraversalBiFunctionArgument {
    genericLiteralArgumentAndTraversalBiFunctionArgumentLiteral :: GenericLiteralArgument,
    genericLiteralArgumentAndTraversalBiFunctionArgumentBiFunction :: TraversalBiFunctionArgument}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndTraversalBiFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument")

_GenericLiteralArgumentAndTraversalBiFunctionArgument_literal = (Core.Name "literal")

_GenericLiteralArgumentAndTraversalBiFunctionArgument_biFunction = (Core.Name "biFunction")

_GenericLiteralArgumentAndTraversalBiFunctionArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "biFunction"),
      Core.fieldTypeType = _TraversalBiFunctionArgument_type_}]}))

data FromArgs = 
  FromArgsString StringArgument |
  FromArgsVertex StructureVertexArgument |
  FromArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_FromArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.FromArgs")

_FromArgs_string = (Core.Name "string")

_FromArgs_vertex = (Core.Name "vertex")

_FromArgs_traversal = (Core.Name "traversal")

_FromArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.FromArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertex"),
      Core.fieldTypeType = _StructureVertexArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data HasArgs = 
  HasArgsString HasStringArgumentAndOptionalStringLiteralVarargs |
  HasArgsTraversalToken HasTraversalTokenArgs
  deriving (Eq, Ord, Read, Show)

_HasArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.HasArgs")

_HasArgs_string = (Core.Name "string")

_HasArgs_traversalToken = (Core.Name "traversalToken")

_HasArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.HasArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _HasStringArgumentAndOptionalStringLiteralVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalToken"),
      Core.fieldTypeType = _HasTraversalTokenArgs_type_}]}))

data HasStringArgumentAndOptionalStringLiteralVarargs = 
  HasStringArgumentAndOptionalStringLiteralVarargs {
    hasStringArgumentAndOptionalStringLiteralVarargsString :: StringNullableArgument,
    hasStringArgumentAndOptionalStringLiteralVarargsRest :: (Maybe HasStringArgumentAndOptionalStringLiteralVarargsRest)}
  deriving (Eq, Ord, Read, Show)

_HasStringArgumentAndOptionalStringLiteralVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargs")

_HasStringArgumentAndOptionalStringLiteralVarargs_string = (Core.Name "string")

_HasStringArgumentAndOptionalStringLiteralVarargs_rest = (Core.Name "rest")

_HasStringArgumentAndOptionalStringLiteralVarargs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = (Core.TypeOptional _HasStringArgumentAndOptionalStringLiteralVarargsRest_type_)}]}))

data HasStringArgumentAndOptionalStringLiteralVarargsRest = 
  HasStringArgumentAndOptionalStringLiteralVarargsRestObject GenericLiteralArgument |
  HasStringArgumentAndOptionalStringLiteralVarargsRestPredicate TraversalPredicate |
  HasStringArgumentAndOptionalStringLiteralVarargsRestStringObject StringNullableArgumentAndGenericLiteralArgument |
  HasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate StringNullableArgumentAndTraversalPredicate |
  HasStringArgumentAndOptionalStringLiteralVarargsRestTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_HasStringArgumentAndOptionalStringLiteralVarargsRest = (Core.Name "hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_object = (Core.Name "object")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_predicate = (Core.Name "predicate")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_stringObject = (Core.Name "stringObject")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_stringPredicate = (Core.Name "stringPredicate")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_traversal = (Core.Name "traversal")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "stringObject"),
      Core.fieldTypeType = _StringNullableArgumentAndGenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "stringPredicate"),
      Core.fieldTypeType = _StringNullableArgumentAndTraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data StringNullableArgumentAndGenericLiteralArgument = 
  StringNullableArgumentAndGenericLiteralArgument {
    stringNullableArgumentAndGenericLiteralArgumentString :: StringNullableArgument,
    stringNullableArgumentAndGenericLiteralArgumentLiteral :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_StringNullableArgumentAndGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndGenericLiteralArgument")

_StringNullableArgumentAndGenericLiteralArgument_string = (Core.Name "string")

_StringNullableArgumentAndGenericLiteralArgument_literal = (Core.Name "literal")

_StringNullableArgumentAndGenericLiteralArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndGenericLiteralArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _GenericLiteralArgument_type_}]}))

data StringNullableArgumentAndTraversalPredicate = 
  StringNullableArgumentAndTraversalPredicate {
    stringNullableArgumentAndTraversalPredicateString :: StringNullableArgument,
    stringNullableArgumentAndTraversalPredicatePredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_StringNullableArgumentAndTraversalPredicate = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndTraversalPredicate")

_StringNullableArgumentAndTraversalPredicate_string = (Core.Name "string")

_StringNullableArgumentAndTraversalPredicate_predicate = (Core.Name "predicate")

_StringNullableArgumentAndTraversalPredicate_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndTraversalPredicate"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_}]}))

data HasTraversalTokenArgs = 
  HasTraversalTokenArgs {
    hasTraversalTokenArgsTraversalToken :: TraversalTokenArgument,
    hasTraversalTokenArgsRest :: HasTraversalTokenArgsRest}
  deriving (Eq, Ord, Read, Show)

_HasTraversalTokenArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.HasTraversalTokenArgs")

_HasTraversalTokenArgs_traversalToken = (Core.Name "traversalToken")

_HasTraversalTokenArgs_rest = (Core.Name "rest")

_HasTraversalTokenArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.HasTraversalTokenArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalToken"),
      Core.fieldTypeType = _TraversalTokenArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = _HasTraversalTokenArgsRest_type_}]}))

data HasTraversalTokenArgsRest = 
  HasTraversalTokenArgsRestLiteral GenericLiteralArgument |
  HasTraversalTokenArgsRestPredicate TraversalPredicate |
  HasTraversalTokenArgsRestTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_HasTraversalTokenArgsRest = (Core.Name "hydra/langs/tinkerpop/gremlin.HasTraversalTokenArgsRest")

_HasTraversalTokenArgsRest_literal = (Core.Name "literal")

_HasTraversalTokenArgsRest_predicate = (Core.Name "predicate")

_HasTraversalTokenArgsRest_traversal = (Core.Name "traversal")

_HasTraversalTokenArgsRest_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.HasTraversalTokenArgsRest"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data GenericLiteralArgumentAndTraversalPredicate = 
  GenericLiteralArgumentAndTraversalPredicateLiteral GenericLiteralArgument |
  GenericLiteralArgumentAndTraversalPredicatePredicate TraversalPredicate
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndTraversalPredicate = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalPredicate")

_GenericLiteralArgumentAndTraversalPredicate_literal = (Core.Name "literal")

_GenericLiteralArgumentAndTraversalPredicate_predicate = (Core.Name "predicate")

_GenericLiteralArgumentAndTraversalPredicate_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalPredicate"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_}]}))

data TraversalPredicateOrStringLiteralVarargs = 
  TraversalPredicateOrStringLiteralVarargsPredicate TraversalPredicate |
  TraversalPredicateOrStringLiteralVarargsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateOrStringLiteralVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateOrStringLiteralVarargs")

_TraversalPredicateOrStringLiteralVarargs_predicate = (Core.Name "predicate")

_TraversalPredicateOrStringLiteralVarargs_string = (Core.Name "string")

_TraversalPredicateOrStringLiteralVarargs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateOrStringLiteralVarargs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)}]}))

data TraversalPredicateOrGenericLiteralArgument = 
  TraversalPredicateOrGenericLiteralArgumentPredicate TraversalPredicate |
  TraversalPredicateOrGenericLiteralArgumentLiteral [GenericLiteralArgument]
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateOrGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateOrGenericLiteralArgument")

_TraversalPredicateOrGenericLiteralArgument_predicate = (Core.Name "predicate")

_TraversalPredicateOrGenericLiteralArgument_literal = (Core.Name "literal")

_TraversalPredicateOrGenericLiteralArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateOrGenericLiteralArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)}]}))

data OptionArgs = 
  OptionArgsPredicateTraversal TraversalPredicateAndNestedTraversal |
  OptionArgsMergeMap TraversalMergeArgumentAndGenericLiteralMapNullableArgument |
  OptionArgsMergeTraversal TraversalMergeArgumentAndNestedTraversal |
  OptionArgsObjectTraversal GenericLiteralArgumentAndNestedTraversal |
  OptionArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_OptionArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionArgs")

_OptionArgs_predicateTraversal = (Core.Name "predicateTraversal")

_OptionArgs_mergeMap = (Core.Name "mergeMap")

_OptionArgs_mergeTraversal = (Core.Name "mergeTraversal")

_OptionArgs_objectTraversal = (Core.Name "objectTraversal")

_OptionArgs_traversal = (Core.Name "traversal")

_OptionArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicateTraversal"),
      Core.fieldTypeType = _TraversalPredicateAndNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mergeMap"),
      Core.fieldTypeType = _TraversalMergeArgumentAndGenericLiteralMapNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mergeTraversal"),
      Core.fieldTypeType = _TraversalMergeArgumentAndNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectTraversal"),
      Core.fieldTypeType = _GenericLiteralArgumentAndNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data TraversalPredicateAndNestedTraversal = 
  TraversalPredicateAndNestedTraversal {
    traversalPredicateAndNestedTraversalPredicate :: TraversalPredicate,
    traversalPredicateAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateAndNestedTraversal")

_TraversalPredicateAndNestedTraversal_predicate = (Core.Name "predicate")

_TraversalPredicateAndNestedTraversal_traversal = (Core.Name "traversal")

_TraversalPredicateAndNestedTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateAndNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data TraversalMergeArgumentAndGenericLiteralMapNullableArgument = 
  TraversalMergeArgumentAndGenericLiteralMapNullableArgument {
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentMerge :: TraversalMergeArgument,
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentMap :: GenericLiteralMapNullableArgument,
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentCardinality :: (Maybe TraversalCardinality)}
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_merge = (Core.Name "merge")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_map = (Core.Name "map")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_cardinality = (Core.Name "cardinality")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "merge"),
      Core.fieldTypeType = _TraversalMergeArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _GenericLiteralMapNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cardinality"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalCardinality_type_)}]}))

data TraversalMergeArgumentAndNestedTraversal = 
  TraversalMergeArgumentAndNestedTraversal {
    traversalMergeArgumentAndNestedTraversalMerge :: TraversalMergeArgument,
    traversalMergeArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndNestedTraversal")

_TraversalMergeArgumentAndNestedTraversal_merge = (Core.Name "merge")

_TraversalMergeArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

_TraversalMergeArgumentAndNestedTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "merge"),
      Core.fieldTypeType = _TraversalMergeArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data GenericLiteralArgumentAndNestedTraversal = 
  GenericLiteralArgumentAndNestedTraversal {
    genericLiteralArgumentAndNestedTraversalObject :: GenericLiteralArgument,
    genericLiteralArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndNestedTraversal")

_GenericLiteralArgumentAndNestedTraversal_object = (Core.Name "object")

_GenericLiteralArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

_GenericLiteralArgumentAndNestedTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data PropertyArgs = 
  PropertyArgsCardinalityObjects TraversalCardinalityArgumentAndObjects |
  PropertyArgsObjects [GenericLiteralArgument] |
  PropertyArgsObject GenericLiteralMapNullableArgument |
  PropertyArgsCardinalityObject GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
  deriving (Eq, Ord, Read, Show)

_PropertyArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.PropertyArgs")

_PropertyArgs_cardinalityObjects = (Core.Name "cardinalityObjects")

_PropertyArgs_objects = (Core.Name "objects")

_PropertyArgs_object = (Core.Name "object")

_PropertyArgs_cardinalityObject = (Core.Name "cardinalityObject")

_PropertyArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.PropertyArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cardinalityObjects"),
      Core.fieldTypeType = _TraversalCardinalityArgumentAndObjects_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objects"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _GenericLiteralMapNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cardinalityObject"),
      Core.fieldTypeType = _GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_type_}]}))

data TraversalCardinalityArgumentAndObjects = 
  TraversalCardinalityArgumentAndObjects {
    traversalCardinalityArgumentAndObjectsCardinality :: TraversalCardinalityArgument,
    traversalCardinalityArgumentAndObjectsObjects :: [GenericLiteralArgument]}
  deriving (Eq, Ord, Read, Show)

_TraversalCardinalityArgumentAndObjects = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinalityArgumentAndObjects")

_TraversalCardinalityArgumentAndObjects_cardinality = (Core.Name "cardinality")

_TraversalCardinalityArgumentAndObjects_objects = (Core.Name "objects")

_TraversalCardinalityArgumentAndObjects_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinalityArgumentAndObjects"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cardinality"),
      Core.fieldTypeType = _TraversalCardinalityArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objects"),
      Core.fieldTypeType = (Core.TypeList _GenericLiteralArgument_type_)}]}))

data GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument = 
  GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument {
    genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentCardinality :: TraversalCardinalityArgument,
    genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentObject :: GenericLiteralMapNullableArgument}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument")

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_cardinality = (Core.Name "cardinality")

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_object = (Core.Name "object")

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cardinality"),
      Core.fieldTypeType = _TraversalCardinalityArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _GenericLiteralMapNullableArgument_type_}]}))

data RangeArgs = 
  RangeArgs {
    rangeArgsScope :: (Maybe TraversalScopeArgument),
    rangeArgsMin :: IntegerArgument,
    rangeArgsMax :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_RangeArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.RangeArgs")

_RangeArgs_scope = (Core.Name "scope")

_RangeArgs_min = (Core.Name "min")

_RangeArgs_max = (Core.Name "max")

_RangeArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.RangeArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "min"),
      Core.fieldTypeType = _IntegerArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "max"),
      Core.fieldTypeType = _IntegerArgument_type_}]}))

data OptionalStringArgumentAndNestedTraversal = 
  OptionalStringArgumentAndNestedTraversal {
    optionalStringArgumentAndNestedTraversalString :: (Maybe StringArgument),
    optionalStringArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_OptionalStringArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalStringArgumentAndNestedTraversal")

_OptionalStringArgumentAndNestedTraversal_string = (Core.Name "string")

_OptionalStringArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

_OptionalStringArgumentAndNestedTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalStringArgumentAndNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data SelectArgs = 
  SelectArgsColumn TraversalColumnArgument |
  SelectArgsPopStrings PopStringsArgument |
  SelectArgsPopTraversal TraversalPopArgumentAndNestedTraversal |
  SelectArgsStrings [StringArgument] |
  SelectArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_SelectArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.SelectArgs")

_SelectArgs_column = (Core.Name "column")

_SelectArgs_popStrings = (Core.Name "popStrings")

_SelectArgs_popTraversal = (Core.Name "popTraversal")

_SelectArgs_strings = (Core.Name "strings")

_SelectArgs_traversal = (Core.Name "traversal")

_SelectArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.SelectArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _TraversalColumnArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "popStrings"),
      Core.fieldTypeType = _PopStringsArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "popTraversal"),
      Core.fieldTypeType = _TraversalPopArgumentAndNestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "strings"),
      Core.fieldTypeType = (Core.TypeList _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data PopStringsArgument = 
  PopStringsArgument {
    popStringsArgumentPop :: TraversalPopArgument,
    popStringsArgumentString :: [StringArgument]}
  deriving (Eq, Ord, Read, Show)

_PopStringsArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.PopStringsArgument")

_PopStringsArgument_pop = (Core.Name "pop")

_PopStringsArgument_string = (Core.Name "string")

_PopStringsArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.PopStringsArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pop"),
      Core.fieldTypeType = _TraversalPopArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeList _StringArgument_type_)}]}))

data TraversalPopArgumentAndNestedTraversal = 
  TraversalPopArgumentAndNestedTraversal {
    traversalPopArgumentAndNestedTraversalPop :: TraversalPopArgument,
    traversalPopArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalPopArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPopArgumentAndNestedTraversal")

_TraversalPopArgumentAndNestedTraversal_pop = (Core.Name "pop")

_TraversalPopArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

_TraversalPopArgumentAndNestedTraversal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPopArgumentAndNestedTraversal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pop"),
      Core.fieldTypeType = _TraversalPopArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data OptionalTraversalScopeArgumentAndIntegerArgument = 
  OptionalTraversalScopeArgumentAndIntegerArgument {
    optionalTraversalScopeArgumentAndIntegerArgumentScope :: (Maybe TraversalScopeArgument),
    optionalTraversalScopeArgumentAndIntegerArgumentLong :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_OptionalTraversalScopeArgumentAndIntegerArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndIntegerArgument")

_OptionalTraversalScopeArgumentAndIntegerArgument_scope = (Core.Name "scope")

_OptionalTraversalScopeArgumentAndIntegerArgument_long = (Core.Name "long")

_OptionalTraversalScopeArgumentAndIntegerArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndIntegerArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "long"),
      Core.fieldTypeType = _IntegerArgument_type_}]}))

data TailArgs = 
  TailArgs {
    tailArgsScope :: (Maybe TraversalScopeArgument),
    tailArgsInteger :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)

_TailArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.TailArgs")

_TailArgs_scope = (Core.Name "scope")

_TailArgs_integer = (Core.Name "integer")

_TailArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TailArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = (Core.TypeOptional _IntegerArgument_type_)}]}))

data ToArgs = 
  ToArgsDirection DirectionAndVarargs |
  ToArgsString StringArgument |
  ToArgsVertex StructureVertexArgument |
  ToArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_ToArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ToArgs")

_ToArgs_direction = (Core.Name "direction")

_ToArgs_string = (Core.Name "string")

_ToArgs_vertex = (Core.Name "vertex")

_ToArgs_traversal = (Core.Name "traversal")

_ToArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ToArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "direction"),
      Core.fieldTypeType = _DirectionAndVarargs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vertex"),
      Core.fieldTypeType = _StructureVertexArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data DirectionAndVarargs = 
  DirectionAndVarargs {
    directionAndVarargsDirection :: TraversalDirectionArgument,
    directionAndVarargsVarargs :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_DirectionAndVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.DirectionAndVarargs")

_DirectionAndVarargs_direction = (Core.Name "direction")

_DirectionAndVarargs_varargs = (Core.Name "varargs")

_DirectionAndVarargs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.DirectionAndVarargs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "direction"),
      Core.fieldTypeType = _TraversalDirectionArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "varargs"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)}]}))

data ValueMapArgs = 
  ValueMapArgsString [StringNullableArgument] |
  ValueMapArgsBoolean ValueMapBooleanArgs
  deriving (Eq, Ord, Read, Show)

_ValueMapArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ValueMapArgs")

_ValueMapArgs_string = (Core.Name "string")

_ValueMapArgs_boolean = (Core.Name "boolean")

_ValueMapArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ValueMapArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = _ValueMapBooleanArgs_type_}]}))

data ValueMapBooleanArgs = 
  ValueMapBooleanArgs {
    valueMapBooleanArgsValue :: BooleanArgument,
    valueMapBooleanArgsKeys :: (Maybe [StringNullableArgument])}
  deriving (Eq, Ord, Read, Show)

_ValueMapBooleanArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ValueMapBooleanArgs")

_ValueMapBooleanArgs_value = (Core.Name "value")

_ValueMapBooleanArgs_keys = (Core.Name "keys")

_ValueMapBooleanArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ValueMapBooleanArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _BooleanArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keys"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeList _StringNullableArgument_type_))}]}))

data WhereArgs = 
  WhereArgsPredicate WhereWithPredicateArgs |
  WhereArgsString StringArgument |
  WhereArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_WhereArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.WhereArgs")

_WhereArgs_predicate = (Core.Name "predicate")

_WhereArgs_string = (Core.Name "string")

_WhereArgs_traversal = (Core.Name "traversal")

_WhereArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.WhereArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _WhereWithPredicateArgs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_}]}))

data WhereWithPredicateArgs = 
  WhereWithPredicateArgs {
    whereWithPredicateArgsLeftArg :: (Maybe StringArgument),
    whereWithPredicateArgsPredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_WhereWithPredicateArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.WhereWithPredicateArgs")

_WhereWithPredicateArgs_leftArg = (Core.Name "leftArg")

_WhereWithPredicateArgs_predicate = (Core.Name "predicate")

_WhereWithPredicateArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.WhereWithPredicateArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "leftArg"),
      Core.fieldTypeType = (Core.TypeOptional _StringArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _TraversalPredicate_type_}]}))

data WithArgs = 
  WithArgs {
    withArgsKeys :: WithArgsKeys,
    withArgsValues :: (Maybe WithArgsValues)}
  deriving (Eq, Ord, Read, Show)

_WithArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgs")

_WithArgs_keys = (Core.Name "keys")

_WithArgs_values = (Core.Name "values")

_WithArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keys"),
      Core.fieldTypeType = _WithArgsKeys_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "values"),
      Core.fieldTypeType = (Core.TypeOptional _WithArgsValues_type_)}]}))

data WithArgsKeys = 
  WithArgsKeysWithOption WithOptionKeys |
  WithArgsKeysString StringArgument
  deriving (Eq, Ord, Read, Show)

_WithArgsKeys = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgsKeys")

_WithArgsKeys_withOption = (Core.Name "withOption")

_WithArgsKeys_string = (Core.Name "string")

_WithArgsKeys_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgsKeys"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withOption"),
      Core.fieldTypeType = _WithOptionKeys_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringArgument_type_}]}))

data WithArgsValues = 
  WithArgsValuesWithOptions WithOptionsValues |
  WithArgsValuesIo IoOptionsValues |
  WithArgsValuesObject GenericLiteralArgument
  deriving (Eq, Ord, Read, Show)

_WithArgsValues = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgsValues")

_WithArgsValues_withOptions = (Core.Name "withOptions")

_WithArgsValues_io = (Core.Name "io")

_WithArgsValues_object = (Core.Name "object")

_WithArgsValues_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgsValues"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withOptions"),
      Core.fieldTypeType = _WithOptionsValues_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "io"),
      Core.fieldTypeType = _IoOptionsValues_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _GenericLiteralArgument_type_}]}))

data ConcatArgs = 
  ConcatArgsTraversal [NestedTraversal] |
  ConcatArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_ConcatArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ConcatArgs")

_ConcatArgs_traversal = (Core.Name "traversal")

_ConcatArgs_string = (Core.Name "string")

_ConcatArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ConcatArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = (Core.TypeList _NestedTraversal_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeList _StringNullableArgument_type_)}]}))

data ReplaceArgs = 
  ReplaceArgs {
    replaceArgsScope :: (Maybe TraversalScopeArgument),
    replaceArgsFrom :: StringNullableArgument,
    replaceArgsTo :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)

_ReplaceArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ReplaceArgs")

_ReplaceArgs_scope = (Core.Name "scope")

_ReplaceArgs_from = (Core.Name "from")

_ReplaceArgs_to = (Core.Name "to")

_ReplaceArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ReplaceArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "from"),
      Core.fieldTypeType = _StringNullableArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "to"),
      Core.fieldTypeType = _StringNullableArgument_type_}]}))

data SplitArgs = 
  SplitArgs {
    splitArgsScope :: (Maybe TraversalScopeArgument),
    splitArgsDelimiter :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)

_SplitArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.SplitArgs")

_SplitArgs_scope = (Core.Name "scope")

_SplitArgs_delimiter = (Core.Name "delimiter")

_SplitArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.SplitArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "delimiter"),
      Core.fieldTypeType = _StringNullableArgument_type_}]}))

data SubstringArgs = 
  SubstringArgs {
    substringArgsScope :: (Maybe TraversalScopeArgument),
    substringArgsStart :: IntegerArgument,
    substringArgsEnd :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)

_SubstringArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.SubstringArgs")

_SubstringArgs_scope = (Core.Name "scope")

_SubstringArgs_start = (Core.Name "start")

_SubstringArgs_end = (Core.Name "end")

_SubstringArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.SubstringArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TraversalScopeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "start"),
      Core.fieldTypeType = _IntegerArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "end"),
      Core.fieldTypeType = (Core.TypeOptional _IntegerArgument_type_)}]}))

data DateAddArgs = 
  DateAddArgs {
    dateAddArgsUnit :: TraversalDTArgument,
    dateAddArgsDuration :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_DateAddArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.DateAddArgs")

_DateAddArgs_unit = (Core.Name "unit")

_DateAddArgs_duration = (Core.Name "duration")

_DateAddArgs_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.DateAddArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unit"),
      Core.fieldTypeType = _TraversalDTArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "duration"),
      Core.fieldTypeType = _IntegerArgument_type_}]}))

data DateDiffArgs = 
  DateDiffArgsTraversal NestedTraversal |
  DateDiffArgsDate DateArgument
  deriving (Eq, Ord, Read, Show)

_DateDiffArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.DateDiffArgs")

_DateDiffArgs_traversal = (Core.Name "traversal")

_DateDiffArgs_date = (Core.Name "date")

_DateDiffArgs_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.DateDiffArgs"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversal"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "date"),
      Core.fieldTypeType = _DateArgument_type_}]}))

data StructureVertex = 
  StructureVertex {
    structureVertexNew :: Bool,
    structureVertexId :: GenericLiteralArgument,
    structureVertexLabel :: StringArgument}
  deriving (Eq, Ord, Read, Show)

_StructureVertex = (Core.Name "hydra/langs/tinkerpop/gremlin.StructureVertex")

_StructureVertex_new = (Core.Name "new")

_StructureVertex_id = (Core.Name "id")

_StructureVertex_label = (Core.Name "label")

_StructureVertex_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StructureVertex"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "new"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = _StringArgument_type_}]}))

data TraversalStrategy = 
  TraversalStrategy {
    traversalStrategyNew :: Bool,
    traversalStrategyClass :: Identifier,
    traversalStrategyConfigurations :: [Configuration]}
  deriving (Eq, Ord, Read, Show)

_TraversalStrategy = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalStrategy")

_TraversalStrategy_new = (Core.Name "new")

_TraversalStrategy_class = (Core.Name "class")

_TraversalStrategy_configurations = (Core.Name "configurations")

_TraversalStrategy_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalStrategy"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "new"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "configurations"),
      Core.fieldTypeType = (Core.TypeList _Configuration_type_)}]}))

data Configuration = 
  Configuration {
    configurationKey :: KeywordOrIdentifier,
    configurationValue :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_Configuration = (Core.Name "hydra/langs/tinkerpop/gremlin.Configuration")

_Configuration_key = (Core.Name "key")

_Configuration_value = (Core.Name "value")

_Configuration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.Configuration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = _KeywordOrIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _GenericLiteralArgument_type_}]}))

data KeywordOrIdentifier = 
  KeywordOrIdentifierKeyword Keyword |
  KeywordOrIdentifierIdentifier Identifier
  deriving (Eq, Ord, Read, Show)

_KeywordOrIdentifier = (Core.Name "hydra/langs/tinkerpop/gremlin.KeywordOrIdentifier")

_KeywordOrIdentifier_keyword = (Core.Name "keyword")

_KeywordOrIdentifier_identifier = (Core.Name "identifier")

_KeywordOrIdentifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.KeywordOrIdentifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keyword"),
      Core.fieldTypeType = _Keyword_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalScope = 
  TraversalScopeLocal  |
  TraversalScopeGlobal 
  deriving (Eq, Ord, Read, Show)

_TraversalScope = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalScope")

_TraversalScope_local = (Core.Name "local")

_TraversalScope_global = (Core.Name "global")

_TraversalScope_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalScope"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "local"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "global"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalToken = 
  TraversalTokenId  |
  TraversalTokenLabel  |
  TraversalTokenKey  |
  TraversalTokenValue 
  deriving (Eq, Ord, Read, Show)

_TraversalToken = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalToken")

_TraversalToken_id = (Core.Name "id")

_TraversalToken_label = (Core.Name "label")

_TraversalToken_key = (Core.Name "key")

_TraversalToken_value = (Core.Name "value")

_TraversalToken_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalToken"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "label"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalMerge = 
  TraversalMergeOnCreate  |
  TraversalMergeOnMatch  |
  TraversalMergeOutV  |
  TraversalMergeInV 
  deriving (Eq, Ord, Read, Show)

_TraversalMerge = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMerge")

_TraversalMerge_onCreate = (Core.Name "onCreate")

_TraversalMerge_onMatch = (Core.Name "onMatch")

_TraversalMerge_outV = (Core.Name "outV")

_TraversalMerge_inV = (Core.Name "inV")

_TraversalMerge_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMerge"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "onCreate"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "onMatch"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outV"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inV"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalOrder = 
  TraversalOrderIncr  |
  TraversalOrderDecr  |
  TraversalOrderAsc  |
  TraversalOrderDesc  |
  TraversalOrderShuffle 
  deriving (Eq, Ord, Read, Show)

_TraversalOrder = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOrder")

_TraversalOrder_incr = (Core.Name "incr")

_TraversalOrder_decr = (Core.Name "decr")

_TraversalOrder_asc = (Core.Name "asc")

_TraversalOrder_desc = (Core.Name "desc")

_TraversalOrder_shuffle = (Core.Name "shuffle")

_TraversalOrder_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOrder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "incr"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "decr"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "asc"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "desc"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shuffle"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalDirection = 
  TraversalDirectionIn  |
  TraversalDirectionOut  |
  TraversalDirectionBoth 
  deriving (Eq, Ord, Read, Show)

_TraversalDirection = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDirection")

_TraversalDirection_in = (Core.Name "in")

_TraversalDirection_out = (Core.Name "out")

_TraversalDirection_both = (Core.Name "both")

_TraversalDirection_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDirection"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "in"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "out"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "both"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalCardinality = 
  TraversalCardinalitySingle GenericLiteral |
  TraversalCardinalitySet GenericLiteral |
  TraversalCardinalityList GenericLiteral
  deriving (Eq, Ord, Read, Show)

_TraversalCardinality = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinality")

_TraversalCardinality_single = (Core.Name "single")

_TraversalCardinality_set = (Core.Name "set")

_TraversalCardinality_list = (Core.Name "list")

_TraversalCardinality_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinality"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "single"),
      Core.fieldTypeType = _GenericLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "set"),
      Core.fieldTypeType = _GenericLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _GenericLiteral_type_}]}))

data TraversalColumn = 
  TraversalColumnKeys  |
  TraversalColumnValues 
  deriving (Eq, Ord, Read, Show)

_TraversalColumn = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalColumn")

_TraversalColumn_keys = (Core.Name "keys")

_TraversalColumn_values = (Core.Name "values")

_TraversalColumn_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalColumn"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keys"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "values"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalPop = 
  TraversalPopFirst  |
  TraversalPopLast  |
  TraversalPopAll  |
  TraversalPopMixed 
  deriving (Eq, Ord, Read, Show)

_TraversalPop = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPop")

_TraversalPop_first = (Core.Name "first")

_TraversalPop_last = (Core.Name "last")

_TraversalPop_all = (Core.Name "all")

_TraversalPop_mixed = (Core.Name "mixed")

_TraversalPop_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPop"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "last"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mixed"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalOperator = 
  TraversalOperatorAddAll  |
  TraversalOperatorAnd  |
  TraversalOperatorAssign  |
  TraversalOperatorDiv  |
  TraversalOperatorMax  |
  TraversalOperatorMin  |
  TraversalOperatorMinus  |
  TraversalOperatorMult  |
  TraversalOperatorOr  |
  TraversalOperatorSum  |
  TraversalOperatorSumLong 
  deriving (Eq, Ord, Read, Show)

_TraversalOperator = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOperator")

_TraversalOperator_addAll = (Core.Name "addAll")

_TraversalOperator_and = (Core.Name "and")

_TraversalOperator_assign = (Core.Name "assign")

_TraversalOperator_div = (Core.Name "div")

_TraversalOperator_max = (Core.Name "max")

_TraversalOperator_min = (Core.Name "min")

_TraversalOperator_minus = (Core.Name "minus")

_TraversalOperator_mult = (Core.Name "mult")

_TraversalOperator_or = (Core.Name "or")

_TraversalOperator_sum = (Core.Name "sum")

_TraversalOperator_sumLong = (Core.Name "sumLong")

_TraversalOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "addAll"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "assign"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "div"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "max"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "min"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minus"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mult"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sum"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sumLong"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalPick = 
  TraversalPickAny  |
  TraversalPickNone 
  deriving (Eq, Ord, Read, Show)

_TraversalPick = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPick")

_TraversalPick_any = (Core.Name "any")

_TraversalPick_none = (Core.Name "none")

_TraversalPick_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPick"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "any"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalDT = 
  TraversalDTSecond  |
  TraversalDTMinute  |
  TraversalDTHour  |
  TraversalDTDay 
  deriving (Eq, Ord, Read, Show)

_TraversalDT = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDT")

_TraversalDT_second = (Core.Name "second")

_TraversalDT_minute = (Core.Name "minute")

_TraversalDT_hour = (Core.Name "hour")

_TraversalDT_day = (Core.Name "day")

_TraversalDT_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDT"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "second"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minute"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hour"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "day"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

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
  TraversalPredicateAnd TwoTraversalPredicates |
  TraversalPredicateOr TwoTraversalPredicates |
  TraversalPredicateNegate TraversalPredicate
  deriving (Eq, Ord, Read, Show)

_TraversalPredicate = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicate")

_TraversalPredicate_eq = (Core.Name "eq")

_TraversalPredicate_neq = (Core.Name "neq")

_TraversalPredicate_lt = (Core.Name "lt")

_TraversalPredicate_lte = (Core.Name "lte")

_TraversalPredicate_gt = (Core.Name "gt")

_TraversalPredicate_gte = (Core.Name "gte")

_TraversalPredicate_inside = (Core.Name "inside")

_TraversalPredicate_outside = (Core.Name "outside")

_TraversalPredicate_between = (Core.Name "between")

_TraversalPredicate_within = (Core.Name "within")

_TraversalPredicate_without = (Core.Name "without")

_TraversalPredicate_not = (Core.Name "not")

_TraversalPredicate_startingWith = (Core.Name "startingWith")

_TraversalPredicate_notStartingWith = (Core.Name "notStartingWith")

_TraversalPredicate_endingWith = (Core.Name "endingWith")

_TraversalPredicate_notEndingWith = (Core.Name "notEndingWith")

_TraversalPredicate_containing = (Core.Name "containing")

_TraversalPredicate_notContaining = (Core.Name "notContaining")

_TraversalPredicate_regex = (Core.Name "regex")

_TraversalPredicate_notRegex = (Core.Name "notRegex")

_TraversalPredicate_and = (Core.Name "and")

_TraversalPredicate_or = (Core.Name "or")

_TraversalPredicate_negate = (Core.Name "negate")

_TraversalPredicate_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicate"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "eq"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "neq"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lt"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lte"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gt"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gte"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inside"),
      Core.fieldTypeType = _RangeArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outside"),
      Core.fieldTypeType = _RangeArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "between"),
      Core.fieldTypeType = _RangeArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "within"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "without"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "not"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "startingWith"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "notStartingWith"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "endingWith"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "notEndingWith"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "containing"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "notContaining"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "regex"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "notRegex"),
      Core.fieldTypeType = _StringArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = _TwoTraversalPredicates_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = _TwoTraversalPredicates_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "negate"),
      Core.fieldTypeType = _TraversalPredicate_type_}]}))

data TwoTraversalPredicates = 
  TwoTraversalPredicates {
    twoTraversalPredicatesLeft :: TraversalPredicate,
    twoTraversalPredicatesRight :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_TwoTraversalPredicates = (Core.Name "hydra/langs/tinkerpop/gremlin.TwoTraversalPredicates")

_TwoTraversalPredicates_left = (Core.Name "left")

_TwoTraversalPredicates_right = (Core.Name "right")

_TwoTraversalPredicates_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TwoTraversalPredicates"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _TraversalPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = _TraversalPredicate_type_}]}))

data TraversalTerminalMethod = 
  TraversalTerminalMethodExplain  |
  TraversalTerminalMethodIterate  |
  TraversalTerminalMethodHasNext  |
  TraversalTerminalMethodTryNext  |
  TraversalTerminalMethodNext (Maybe IntegerLiteral) |
  TraversalTerminalMethodToList  |
  TraversalTerminalMethodToSet  |
  TraversalTerminalMethodToBulkSet 
  deriving (Eq, Ord, Read, Show)

_TraversalTerminalMethod = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalTerminalMethod")

_TraversalTerminalMethod_explain = (Core.Name "explain")

_TraversalTerminalMethod_iterate = (Core.Name "iterate")

_TraversalTerminalMethod_hasNext = (Core.Name "hasNext")

_TraversalTerminalMethod_tryNext = (Core.Name "tryNext")

_TraversalTerminalMethod_next = (Core.Name "next")

_TraversalTerminalMethod_toList = (Core.Name "toList")

_TraversalTerminalMethod_toSet = (Core.Name "toSet")

_TraversalTerminalMethod_toBulkSet = (Core.Name "toBulkSet")

_TraversalTerminalMethod_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalTerminalMethod"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "explain"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iterate"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasNext"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tryNext"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "next"),
      Core.fieldTypeType = (Core.TypeOptional _IntegerLiteral_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toList"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toSet"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "toBulkSet"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalSelfMethod = 
  TraversalSelfMethodDiscard 
  deriving (Eq, Ord, Read, Show)

_TraversalSelfMethod = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSelfMethod")

_TraversalSelfMethod_discard = (Core.Name "discard")

_TraversalSelfMethod_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSelfMethod"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "discard"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TraversalFunction = 
  TraversalFunctionToken TraversalToken |
  TraversalFunctionColumn TraversalColumn
  deriving (Eq, Ord, Read, Show)

_TraversalFunction = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunction")

_TraversalFunction_token = (Core.Name "token")

_TraversalFunction_column = (Core.Name "column")

_TraversalFunction_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunction"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "token"),
      Core.fieldTypeType = _TraversalToken_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _TraversalColumn_type_}]}))

data RangeArgument = 
  RangeArgument {
    rangeArgumentMin :: GenericLiteralArgument,
    rangeArgumentMax :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_RangeArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.RangeArgument")

_RangeArgument_min = (Core.Name "min")

_RangeArgument_max = (Core.Name "max")

_RangeArgument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.RangeArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "min"),
      Core.fieldTypeType = _GenericLiteralArgument_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "max"),
      Core.fieldTypeType = _GenericLiteralArgument_type_}]}))

data WithOptionKeys = 
  WithOptionKeysShortestPath ShortestPathConstants |
  WithOptionKeysConnectedComponent ConnectedComponentConstants |
  WithOptionKeysPageRank PageRankConstants |
  WithOptionKeysPeerPressure PeerPressureConstants |
  WithOptionKeysIo IoOptionsKeys |
  WithOptionKeysWithOptionsTokens  |
  WithOptionKeysWithOptionsIndexer 
  deriving (Eq, Ord, Read, Show)

_WithOptionKeys = (Core.Name "hydra/langs/tinkerpop/gremlin.WithOptionKeys")

_WithOptionKeys_shortestPath = (Core.Name "shortestPath")

_WithOptionKeys_connectedComponent = (Core.Name "connectedComponent")

_WithOptionKeys_pageRank = (Core.Name "pageRank")

_WithOptionKeys_peerPressure = (Core.Name "peerPressure")

_WithOptionKeys_io = (Core.Name "io")

_WithOptionKeys_withOptionsTokens = (Core.Name "withOptionsTokens")

_WithOptionKeys_withOptionsIndexer = (Core.Name "withOptionsIndexer")

_WithOptionKeys_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.WithOptionKeys"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shortestPath"),
      Core.fieldTypeType = _ShortestPathConstants_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "connectedComponent"),
      Core.fieldTypeType = _ConnectedComponentConstants_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pageRank"),
      Core.fieldTypeType = _PageRankConstants_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "peerPressure"),
      Core.fieldTypeType = _PeerPressureConstants_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "io"),
      Core.fieldTypeType = _IoOptionsKeys_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withOptionsTokens"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withOptionsIndexer"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ConnectedComponentConstants = 
  ConnectedComponentConstantsComponent  |
  ConnectedComponentConstantsEdges  |
  ConnectedComponentConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_ConnectedComponentConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.ConnectedComponentConstants")

_ConnectedComponentConstants_component = (Core.Name "component")

_ConnectedComponentConstants_edges = (Core.Name "edges")

_ConnectedComponentConstants_propertyName = (Core.Name "propertyName")

_ConnectedComponentConstants_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ConnectedComponentConstants"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "component"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edges"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "propertyName"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data PageRankConstants = 
  PageRankConstantsEdges  |
  PageRankConstantsTimes  |
  PageRankConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_PageRankConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.PageRankConstants")

_PageRankConstants_edges = (Core.Name "edges")

_PageRankConstants_times = (Core.Name "times")

_PageRankConstants_propertyName = (Core.Name "propertyName")

_PageRankConstants_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.PageRankConstants"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edges"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "times"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "propertyName"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data PeerPressureConstants = 
  PeerPressureConstantsEdges  |
  PeerPressureConstantsTimes  |
  PeerPressureConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_PeerPressureConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.PeerPressureConstants")

_PeerPressureConstants_edges = (Core.Name "edges")

_PeerPressureConstants_times = (Core.Name "times")

_PeerPressureConstants_propertyName = (Core.Name "propertyName")

_PeerPressureConstants_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.PeerPressureConstants"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edges"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "times"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "propertyName"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ShortestPathConstants = 
  ShortestPathConstantsTarget  |
  ShortestPathConstantsEdges  |
  ShortestPathConstantsDistance  |
  ShortestPathConstantsMaxDistance  |
  ShortestPathConstantsIncludeEdges 
  deriving (Eq, Ord, Read, Show)

_ShortestPathConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.ShortestPathConstants")

_ShortestPathConstants_target = (Core.Name "target")

_ShortestPathConstants_edges = (Core.Name "edges")

_ShortestPathConstants_distance = (Core.Name "distance")

_ShortestPathConstants_maxDistance = (Core.Name "maxDistance")

_ShortestPathConstants_includeEdges = (Core.Name "includeEdges")

_ShortestPathConstants_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.ShortestPathConstants"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "target"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edges"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "distance"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "maxDistance"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "includeEdges"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data WithOptionsValues = 
  WithOptionsValuesTokens  |
  WithOptionsValuesNone  |
  WithOptionsValuesIds  |
  WithOptionsValuesLabels  |
  WithOptionsValuesKeys  |
  WithOptionsValuesValues  |
  WithOptionsValuesAll  |
  WithOptionsValuesList  |
  WithOptionsValuesMap 
  deriving (Eq, Ord, Read, Show)

_WithOptionsValues = (Core.Name "hydra/langs/tinkerpop/gremlin.WithOptionsValues")

_WithOptionsValues_tokens = (Core.Name "tokens")

_WithOptionsValues_none = (Core.Name "none")

_WithOptionsValues_ids = (Core.Name "ids")

_WithOptionsValues_labels = (Core.Name "labels")

_WithOptionsValues_keys = (Core.Name "keys")

_WithOptionsValues_values = (Core.Name "values")

_WithOptionsValues_all = (Core.Name "all")

_WithOptionsValues_list = (Core.Name "list")

_WithOptionsValues_map = (Core.Name "map")

_WithOptionsValues_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.WithOptionsValues"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tokens"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ids"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labels"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keys"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "values"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data IoOptionsKeys = 
  IoOptionsKeysReader  |
  IoOptionsKeysWriter 
  deriving (Eq, Ord, Read, Show)

_IoOptionsKeys = (Core.Name "hydra/langs/tinkerpop/gremlin.IoOptionsKeys")

_IoOptionsKeys_reader = (Core.Name "reader")

_IoOptionsKeys_writer = (Core.Name "writer")

_IoOptionsKeys_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.IoOptionsKeys"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reader"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "writer"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data IoOptionsValues = 
  IoOptionsValuesGryo  |
  IoOptionsValuesGraphson  |
  IoOptionsValuesGraphml 
  deriving (Eq, Ord, Read, Show)

_IoOptionsValues = (Core.Name "hydra/langs/tinkerpop/gremlin.IoOptionsValues")

_IoOptionsValues_gryo = (Core.Name "gryo")

_IoOptionsValues_graphson = (Core.Name "graphson")

_IoOptionsValues_graphml = (Core.Name "graphml")

_IoOptionsValues_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.IoOptionsValues"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gryo"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "graphson"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "graphml"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data BooleanArgument = 
  BooleanArgumentValue Bool |
  BooleanArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_BooleanArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.BooleanArgument")

_BooleanArgument_value = (Core.Name "value")

_BooleanArgument_variable = (Core.Name "variable")

_BooleanArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.BooleanArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data IntegerArgument = 
  IntegerArgumentValue IntegerLiteral |
  IntegerArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_IntegerArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerArgument")

_IntegerArgument_value = (Core.Name "value")

_IntegerArgument_variable = (Core.Name "variable")

_IntegerArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _IntegerLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data FloatArgument = 
  FloatArgumentValue FloatLiteral |
  FloatArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_FloatArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.FloatArgument")

_FloatArgument_value = (Core.Name "value")

_FloatArgument_variable = (Core.Name "variable")

_FloatArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.FloatArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _FloatLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data StringArgument = 
  StringArgumentValue String |
  StringArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StringArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgument")

_StringArgument_value = (Core.Name "value")

_StringArgument_variable = (Core.Name "variable")

_StringArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data StringNullableArgument = 
  StringNullableArgumentValue (Maybe String) |
  StringNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StringNullableArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgument")

_StringNullableArgument_value = (Core.Name "value")

_StringNullableArgument_variable = (Core.Name "variable")

_StringNullableArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data DateArgument = 
  DateArgumentValue DateLiteral |
  DateArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_DateArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.DateArgument")

_DateArgument_value = (Core.Name "value")

_DateArgument_variable = (Core.Name "variable")

_DateArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.DateArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _DateLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data GenericLiteralArgument = 
  GenericLiteralArgumentValue GenericLiteral |
  GenericLiteralArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgument")

_GenericLiteralArgument_value = (Core.Name "value")

_GenericLiteralArgument_variable = (Core.Name "variable")

_GenericLiteralArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _GenericLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data GenericLiteralListArgument = 
  GenericLiteralListArgumentValue GenericLiteralList |
  GenericLiteralListArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralListArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralListArgument")

_GenericLiteralListArgument_value = (Core.Name "value")

_GenericLiteralListArgument_variable = (Core.Name "variable")

_GenericLiteralListArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralListArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _GenericLiteralList_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data GenericLiteralMapArgument = 
  GenericLiteralMapArgumentValue GenericLiteralMap |
  GenericLiteralMapArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapArgument")

_GenericLiteralMapArgument_value = (Core.Name "value")

_GenericLiteralMapArgument_variable = (Core.Name "variable")

_GenericLiteralMapArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _GenericLiteralMap_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data GenericLiteralMapNullableArgument = 
  GenericLiteralMapNullableArgumentValue (Maybe GenericLiteralMap) |
  GenericLiteralMapNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgument")

_GenericLiteralMapNullableArgument_value = (Core.Name "value")

_GenericLiteralMapNullableArgument_variable = (Core.Name "variable")

_GenericLiteralMapNullableArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeOptional _GenericLiteralMap_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data StructureVertexArgument = 
  StructureVertexArgumentValue StructureVertex |
  StructureVertexArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StructureVertexArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StructureVertexArgument")

_StructureVertexArgument_value = (Core.Name "value")

_StructureVertexArgument_variable = (Core.Name "variable")

_StructureVertexArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StructureVertexArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _StructureVertex_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalCardinalityArgument = 
  TraversalCardinalityArgumentValue TraversalCardinality |
  TraversalCardinalityArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalCardinalityArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinalityArgument")

_TraversalCardinalityArgument_value = (Core.Name "value")

_TraversalCardinalityArgument_variable = (Core.Name "variable")

_TraversalCardinalityArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinalityArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalColumnArgument = 
  TraversalColumnArgumentValue TraversalColumn |
  TraversalColumnArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalColumnArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalColumnArgument")

_TraversalColumnArgument_value = (Core.Name "value")

_TraversalColumnArgument_variable = (Core.Name "variable")

_TraversalColumnArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalColumnArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalColumn_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalDirectionArgument = 
  TraversalDirectionArgumentValue TraversalDirection |
  TraversalDirectionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalDirectionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDirectionArgument")

_TraversalDirectionArgument_value = (Core.Name "value")

_TraversalDirectionArgument_variable = (Core.Name "variable")

_TraversalDirectionArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDirectionArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalDirection_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalMergeArgument = 
  TraversalMergeArgumentValue TraversalMerge |
  TraversalMergeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgument")

_TraversalMergeArgument_value = (Core.Name "value")

_TraversalMergeArgument_variable = (Core.Name "variable")

_TraversalMergeArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalMerge_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalOrderArgument = 
  TraversalOrderArgumentValue TraversalOrder |
  TraversalOrderArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalOrderArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOrderArgument")

_TraversalOrderArgument_value = (Core.Name "value")

_TraversalOrderArgument_variable = (Core.Name "variable")

_TraversalOrderArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOrderArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalOrder_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalPopArgument = 
  TraversalPopArgumentValue TraversalPop |
  TraversalPopArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalPopArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPopArgument")

_TraversalPopArgument_value = (Core.Name "value")

_TraversalPopArgument_variable = (Core.Name "variable")

_TraversalPopArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPopArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalPop_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalSackMethodArgument = 
  TraversalSackMethodArgumentValue  |
  TraversalSackMethodArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalSackMethodArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSackMethodArgument")

_TraversalSackMethodArgument_value = (Core.Name "value")

_TraversalSackMethodArgument_variable = (Core.Name "variable")

_TraversalSackMethodArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSackMethodArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalScopeArgument = 
  TraversalScopeArgumentValue TraversalScope |
  TraversalScopeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalScopeArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalScopeArgument")

_TraversalScopeArgument_value = (Core.Name "value")

_TraversalScopeArgument_variable = (Core.Name "variable")

_TraversalScopeArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalScopeArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalScope_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalTokenArgument = 
  TraversalTokenArgumentValue TraversalToken |
  TraversalTokenArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalTokenArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalTokenArgument")

_TraversalTokenArgument_value = (Core.Name "value")

_TraversalTokenArgument_variable = (Core.Name "variable")

_TraversalTokenArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalTokenArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalToken_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalComparatorArgument = 
  TraversalComparatorArgumentValue TraversalOrder |
  TraversalComparatorArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalComparatorArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalComparatorArgument")

_TraversalComparatorArgument_value = (Core.Name "value")

_TraversalComparatorArgument_variable = (Core.Name "variable")

_TraversalComparatorArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalComparatorArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalOrder_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalFunctionArgument = 
  TraversalFunctionArgumentValue TraversalFunction |
  TraversalFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunctionArgument")

_TraversalFunctionArgument_value = (Core.Name "value")

_TraversalFunctionArgument_variable = (Core.Name "variable")

_TraversalFunctionArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunctionArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalFunction_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalBiFunctionArgument = 
  TraversalBiFunctionArgumentValue TraversalOperator |
  TraversalBiFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalBiFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalBiFunctionArgument")

_TraversalBiFunctionArgument_value = (Core.Name "value")

_TraversalBiFunctionArgument_variable = (Core.Name "variable")

_TraversalBiFunctionArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalBiFunctionArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TraversalDTArgument = 
  TraversalDTArgumentValue TraversalDT |
  TraversalDTArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalDTArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDTArgument")

_TraversalDTArgument_value = (Core.Name "value")

_TraversalDTArgument_variable = (Core.Name "variable")

_TraversalDTArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDTArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TraversalDT_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Identifier_type_}]}))

newtype GenericLiteralList = 
  GenericLiteralList {
    unGenericLiteralList :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralList = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralList")

_GenericLiteralList_type_ = (Core.TypeList _GenericLiteral_type_)

data GenericLiteralRange = 
  GenericLiteralRangeInteger IntegerRange |
  GenericLiteralRangeString StringRange
  deriving (Eq, Ord, Read, Show)

_GenericLiteralRange = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralRange")

_GenericLiteralRange_integer = (Core.Name "integer")

_GenericLiteralRange_string = (Core.Name "string")

_GenericLiteralRange_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralRange"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = _IntegerRange_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringRange_type_}]}))

data IntegerRange = 
  IntegerRange {
    integerRangeLeft :: IntegerLiteral,
    integerRangeRight :: IntegerLiteral}
  deriving (Eq, Ord, Read, Show)

_IntegerRange = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerRange")

_IntegerRange_left = (Core.Name "left")

_IntegerRange_right = (Core.Name "right")

_IntegerRange_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerRange"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _IntegerLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = _IntegerLiteral_type_}]}))

data StringRange = 
  StringRange {
    stringRangeLeft :: String,
    stringRangeRight :: String}
  deriving (Eq, Ord, Read, Show)

_StringRange = (Core.Name "hydra/langs/tinkerpop/gremlin.StringRange")

_StringRange_left = (Core.Name "left")

_StringRange_right = (Core.Name "right")

_StringRange_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.StringRange"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

newtype GenericLiteralSet = 
  GenericLiteralSet {
    unGenericLiteralSet :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralSet = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralSet")

_GenericLiteralSet_type_ = (Core.TypeList _GenericLiteral_type_)

newtype GenericLiteralCollection = 
  GenericLiteralCollection {
    unGenericLiteralCollection :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralCollection = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralCollection")

_GenericLiteralCollection_type_ = (Core.TypeList _GenericLiteral_type_)

data GenericLiteral = 
  GenericLiteralNumeric NumericLiteral |
  GenericLiteralBoolean Bool |
  GenericLiteralString String |
  GenericLiteralDate DateLiteral |
  GenericLiteralNull  |
  GenericLiteralNan  |
  GenericLiteralInf  |
  GenericLiteralTraversalToken TraversalToken |
  GenericLiteralTraversalCardinality TraversalCardinality |
  GenericLiteralTraversalDirection TraversalDirection |
  GenericLiteralTraversalMerge TraversalMerge |
  GenericLiteralTraversalPick TraversalPick |
  GenericLiteralTraversalDT TraversalDT |
  GenericLiteralStructureVertex StructureVertex |
  GenericLiteralGenericLiteralSet GenericLiteralSet |
  GenericLiteralGenericLiteralCollection GenericLiteralCollection |
  GenericLiteralGenericLiteralRange GenericLiteralRange |
  GenericLiteralNestedTraversal NestedTraversal |
  GenericLiteralTerminatedTraversal TerminatedTraversal |
  GenericLiteralGenericLiteralMap GenericLiteralMap
  deriving (Eq, Ord, Read, Show)

_GenericLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteral")

_GenericLiteral_numeric = (Core.Name "numeric")

_GenericLiteral_boolean = (Core.Name "boolean")

_GenericLiteral_string = (Core.Name "string")

_GenericLiteral_date = (Core.Name "date")

_GenericLiteral_null = (Core.Name "null")

_GenericLiteral_nan = (Core.Name "nan")

_GenericLiteral_inf = (Core.Name "inf")

_GenericLiteral_traversalToken = (Core.Name "traversalToken")

_GenericLiteral_traversalCardinality = (Core.Name "traversalCardinality")

_GenericLiteral_traversalDirection = (Core.Name "traversalDirection")

_GenericLiteral_traversalMerge = (Core.Name "traversalMerge")

_GenericLiteral_traversalPick = (Core.Name "traversalPick")

_GenericLiteral_traversalDT = (Core.Name "traversalDT")

_GenericLiteral_structureVertex = (Core.Name "structureVertex")

_GenericLiteral_genericLiteralSet = (Core.Name "genericLiteralSet")

_GenericLiteral_genericLiteralCollection = (Core.Name "genericLiteralCollection")

_GenericLiteral_genericLiteralRange = (Core.Name "genericLiteralRange")

_GenericLiteral_nestedTraversal = (Core.Name "nestedTraversal")

_GenericLiteral_terminatedTraversal = (Core.Name "terminatedTraversal")

_GenericLiteral_genericLiteralMap = (Core.Name "genericLiteralMap")

_GenericLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteral"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numeric"),
      Core.fieldTypeType = _NumericLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "date"),
      Core.fieldTypeType = _DateLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nan"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inf"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalToken"),
      Core.fieldTypeType = _TraversalToken_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalCardinality"),
      Core.fieldTypeType = _TraversalCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalDirection"),
      Core.fieldTypeType = _TraversalDirection_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalMerge"),
      Core.fieldTypeType = _TraversalMerge_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalPick"),
      Core.fieldTypeType = _TraversalPick_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalDT"),
      Core.fieldTypeType = _TraversalDT_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "structureVertex"),
      Core.fieldTypeType = _StructureVertex_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "genericLiteralSet"),
      Core.fieldTypeType = _GenericLiteralSet_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "genericLiteralCollection"),
      Core.fieldTypeType = _GenericLiteralCollection_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "genericLiteralRange"),
      Core.fieldTypeType = _GenericLiteralRange_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nestedTraversal"),
      Core.fieldTypeType = _NestedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "terminatedTraversal"),
      Core.fieldTypeType = _TerminatedTraversal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "genericLiteralMap"),
      Core.fieldTypeType = _GenericLiteralMap_type_}]}))

newtype GenericLiteralMap = 
  GenericLiteralMap {
    unGenericLiteralMap :: [MapEntry]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMap = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMap")

_GenericLiteralMap_type_ = (Core.TypeList _MapEntry_type_)

data MapEntry = 
  MapEntryKey MapKey |
  MapEntryValue GenericLiteral
  deriving (Eq, Ord, Read, Show)

_MapEntry = (Core.Name "hydra/langs/tinkerpop/gremlin.MapEntry")

_MapEntry_key = (Core.Name "key")

_MapEntry_value = (Core.Name "value")

_MapEntry_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.MapEntry"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = _MapKey_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _GenericLiteral_type_}]}))

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

_MapKey = (Core.Name "hydra/langs/tinkerpop/gremlin.MapKey")

_MapKey_string = (Core.Name "string")

_MapKey_numeric = (Core.Name "numeric")

_MapKey_traversalToken = (Core.Name "traversalToken")

_MapKey_traversalDirection = (Core.Name "traversalDirection")

_MapKey_set = (Core.Name "set")

_MapKey_collection = (Core.Name "collection")

_MapKey_map = (Core.Name "map")

_MapKey_keyword = (Core.Name "keyword")

_MapKey_identifier = (Core.Name "identifier")

_MapKey_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.MapKey"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numeric"),
      Core.fieldTypeType = _NumericLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalToken"),
      Core.fieldTypeType = _TraversalToken_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "traversalDirection"),
      Core.fieldTypeType = _TraversalDirection_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "set"),
      Core.fieldTypeType = _GenericLiteralSet_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "collection"),
      Core.fieldTypeType = _GenericLiteralCollection_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _GenericLiteralMap_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keyword"),
      Core.fieldTypeType = _Keyword_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

newtype IntegerLiteral = 
  IntegerLiteral {
    unIntegerLiteral :: Integer}
  deriving (Eq, Ord, Read, Show)

_IntegerLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerLiteral")

_IntegerLiteral_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))

newtype FloatLiteral = 
  FloatLiteral {
    unFloatLiteral :: Double}
  deriving (Eq, Ord, Read, Show)

_FloatLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.FloatLiteral")

_FloatLiteral_type_ = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat))

data NumericLiteral = 
  NumericLiteralInteger IntegerLiteral |
  NumericLiteralFloat FloatLiteral
  deriving (Eq, Ord, Read, Show)

_NumericLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.NumericLiteral")

_NumericLiteral_integer = (Core.Name "integer")

_NumericLiteral_float = (Core.Name "float")

_NumericLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.NumericLiteral"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = _IntegerLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = _FloatLiteral_type_}]}))

newtype DateLiteral = 
  DateLiteral {
    unDateLiteral :: (Maybe StringArgument)}
  deriving (Eq, Ord, Read, Show)

_DateLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.DateLiteral")

_DateLiteral_type_ = (Core.TypeOptional _StringArgument_type_)

data Keyword = 
  KeywordEdges  |
  KeywordKeys  |
  KeywordNew  |
  KeywordValues 
  deriving (Eq, Ord, Read, Show)

_Keyword = (Core.Name "hydra/langs/tinkerpop/gremlin.Keyword")

_Keyword_edges = (Core.Name "edges")

_Keyword_keys = (Core.Name "keys")

_Keyword_new = (Core.Name "new")

_Keyword_values = (Core.Name "values")

_Keyword_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/tinkerpop/gremlin.Keyword"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "edges"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keys"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "new"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "values"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra/langs/tinkerpop/gremlin.Identifier")

_Identifier_type_ = (Core.TypeLiteral Core.LiteralTypeString)