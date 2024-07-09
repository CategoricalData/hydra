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

data Query = 
  QueryTraversalSource TraversalSourceQuery |
  QueryRootTraversal RootTraversalQuery |
  QueryToString  |
  QueryEmpty 
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra/langs/tinkerpop/gremlin.Query")

_Query_traversalSource = (Core.FieldName "traversalSource")

_Query_rootTraversal = (Core.FieldName "rootTraversal")

_Query_toString = (Core.FieldName "toString")

_Query_empty = (Core.FieldName "empty")

data TraversalSourceQuery = 
  TraversalSourceQuery {
    traversalSourceQuerySource :: TraversalSource,
    traversalSourceQueryTransactionPart :: (Maybe TransactionPart)}
  deriving (Eq, Ord, Read, Show)

_TraversalSourceQuery = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSourceQuery")

_TraversalSourceQuery_source = (Core.FieldName "source")

_TraversalSourceQuery_transactionPart = (Core.FieldName "transactionPart")

data RootTraversalQuery = 
  RootTraversalQuery {
    rootTraversalQueryRoot :: RootTraversal,
    rootTraversalQueryTerminalMethod :: (Maybe TraversalTerminalMethod)}
  deriving (Eq, Ord, Read, Show)

_RootTraversalQuery = (Core.Name "hydra/langs/tinkerpop/gremlin.RootTraversalQuery")

_RootTraversalQuery_root = (Core.FieldName "root")

_RootTraversalQuery_terminalMethod = (Core.FieldName "terminalMethod")

newtype TraversalSource = 
  TraversalSource {
    unTraversalSource :: [TraversalSourceSelfMethod]}
  deriving (Eq, Ord, Read, Show)

_TraversalSource = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSource")

data TransactionPart = 
  TransactionPartBegin  |
  TransactionPartCommit  |
  TransactionPartRollback 
  deriving (Eq, Ord, Read, Show)

_TransactionPart = (Core.Name "hydra/langs/tinkerpop/gremlin.TransactionPart")

_TransactionPart_begin = (Core.FieldName "begin")

_TransactionPart_commit = (Core.FieldName "commit")

_TransactionPart_rollback = (Core.FieldName "rollback")

data RootTraversal = 
  RootTraversal {
    rootTraversalSource :: TraversalSource,
    rootTraversalSpawnMethod :: TraversalSourceSpawnMethod,
    rootTraversalChained :: [ChainedTraversalElement]}
  deriving (Eq, Ord, Read, Show)

_RootTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.RootTraversal")

_RootTraversal_source = (Core.FieldName "source")

_RootTraversal_spawnMethod = (Core.FieldName "spawnMethod")

_RootTraversal_chained = (Core.FieldName "chained")

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

_TraversalSourceSelfMethod_withBulk = (Core.FieldName "withBulk")

_TraversalSourceSelfMethod_withPath = (Core.FieldName "withPath")

_TraversalSourceSelfMethod_withSack = (Core.FieldName "withSack")

_TraversalSourceSelfMethod_withSideEffect = (Core.FieldName "withSideEffect")

_TraversalSourceSelfMethod_withStrategies = (Core.FieldName "withStrategies")

_TraversalSourceSelfMethod_withoutStrategies = (Core.FieldName "withoutStrategies")

_TraversalSourceSelfMethod_with = (Core.FieldName "with")

data GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument = 
  GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument {
    genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentLiteral :: GenericLiteralArgument,
    genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentBiFunction :: (Maybe TraversalBiFunctionArgument)}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument")

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_literal = (Core.FieldName "literal")

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_biFunction = (Core.FieldName "biFunction")

data StringArgumentAndGenericLiteralArgument = 
  StringArgumentAndGenericLiteralArgument {
    stringArgumentAndGenericLiteralArgumentString :: StringArgument,
    stringArgumentAndGenericLiteralArgumentLiteral :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndGenericLiteralArgument")

_StringArgumentAndGenericLiteralArgument_string = (Core.FieldName "string")

_StringArgumentAndGenericLiteralArgument_literal = (Core.FieldName "literal")

data StringArgumentAndOptionalGenericLiteralArgument = 
  StringArgumentAndOptionalGenericLiteralArgument {
    stringArgumentAndOptionalGenericLiteralArgumentString :: StringArgument,
    stringArgumentAndOptionalGenericLiteralArgumentLiteral :: (Maybe GenericLiteralArgument)}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndOptionalGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndOptionalGenericLiteralArgument")

_StringArgumentAndOptionalGenericLiteralArgument_string = (Core.FieldName "string")

_StringArgumentAndOptionalGenericLiteralArgument_literal = (Core.FieldName "literal")

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

_TraversalSourceSpawnMethod_addE = (Core.FieldName "addE")

_TraversalSourceSpawnMethod_addV = (Core.FieldName "addV")

_TraversalSourceSpawnMethod_e = (Core.FieldName "e")

_TraversalSourceSpawnMethod_v = (Core.FieldName "v")

_TraversalSourceSpawnMethod_mergeV = (Core.FieldName "mergeV")

_TraversalSourceSpawnMethod_mergeE = (Core.FieldName "mergeE")

_TraversalSourceSpawnMethod_inject = (Core.FieldName "inject")

_TraversalSourceSpawnMethod_io = (Core.FieldName "io")

_TraversalSourceSpawnMethod_call = (Core.FieldName "call")

_TraversalSourceSpawnMethod_union = (Core.FieldName "union")

data GenericLiteralMapNullableArgumentOrNestedTraversal = 
  GenericLiteralMapNullableArgumentOrNestedTraversalMap GenericLiteralMapNullableArgument |
  GenericLiteralMapNullableArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgumentOrNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal")

_GenericLiteralMapNullableArgumentOrNestedTraversal_map = (Core.FieldName "map")

_GenericLiteralMapNullableArgumentOrNestedTraversal_traversal = (Core.FieldName "traversal")

data ServiceCall = 
  ServiceCall {
    serviceCallService :: StringArgument,
    serviceCallArguments :: ServiceArguments}
  deriving (Eq, Ord, Read, Show)

_ServiceCall = (Core.Name "hydra/langs/tinkerpop/gremlin.ServiceCall")

_ServiceCall_service = (Core.FieldName "service")

_ServiceCall_arguments = (Core.FieldName "arguments")

data ServiceArguments = 
  ServiceArgumentsMap (Maybe GenericLiteralMapArgument) |
  ServiceArgumentsTraversal (Maybe NestedTraversal)
  deriving (Eq, Ord, Read, Show)

_ServiceArguments = (Core.Name "hydra/langs/tinkerpop/gremlin.ServiceArguments")

_ServiceArguments_map = (Core.FieldName "map")

_ServiceArguments_traversal = (Core.FieldName "traversal")

data ChainedTraversal = 
  ChainedTraversal {
    chainedTraversalFirst :: TraversalMethod,
    chainedTraversalRest :: ChainedTraversalElement}
  deriving (Eq, Ord, Read, Show)

_ChainedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.ChainedTraversal")

_ChainedTraversal_first = (Core.FieldName "first")

_ChainedTraversal_rest = (Core.FieldName "rest")

data ChainedTraversalElement = 
  ChainedTraversalElementMethod TraversalMethod |
  ChainedTraversalElementSelf TraversalSelfMethod
  deriving (Eq, Ord, Read, Show)

_ChainedTraversalElement = (Core.Name "hydra/langs/tinkerpop/gremlin.ChainedTraversalElement")

_ChainedTraversalElement_method = (Core.FieldName "method")

_ChainedTraversalElement_self = (Core.FieldName "self")

data NestedTraversal = 
  NestedTraversalRoot RootTraversal |
  NestedTraversalChained ChainedTraversal |
  NestedTraversalAnonymous ChainedTraversal
  deriving (Eq, Ord, Read, Show)

_NestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.NestedTraversal")

_NestedTraversal_root = (Core.FieldName "root")

_NestedTraversal_chained = (Core.FieldName "chained")

_NestedTraversal_anonymous = (Core.FieldName "anonymous")

data TerminatedTraversal = 
  TerminatedTraversal {
    terminatedTraversalRoot :: RootTraversal,
    terminatedTraversalTerminal :: TraversalTerminalMethod}
  deriving (Eq, Ord, Read, Show)

_TerminatedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TerminatedTraversal")

_TerminatedTraversal_root = (Core.FieldName "root")

_TerminatedTraversal_terminal = (Core.FieldName "terminal")

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

_TraversalMethod_v = (Core.FieldName "v")

_TraversalMethod_e = (Core.FieldName "e")

_TraversalMethod_addE = (Core.FieldName "addE")

_TraversalMethod_addV = (Core.FieldName "addV")

_TraversalMethod_mergeE = (Core.FieldName "mergeE")

_TraversalMethod_mergeV = (Core.FieldName "mergeV")

_TraversalMethod_aggregate = (Core.FieldName "aggregate")

_TraversalMethod_all = (Core.FieldName "all")

_TraversalMethod_and = (Core.FieldName "and")

_TraversalMethod_any = (Core.FieldName "any")

_TraversalMethod_as = (Core.FieldName "as")

_TraversalMethod_barrier = (Core.FieldName "barrier")

_TraversalMethod_both = (Core.FieldName "both")

_TraversalMethod_bothE = (Core.FieldName "bothE")

_TraversalMethod_bothV = (Core.FieldName "bothV")

_TraversalMethod_branch = (Core.FieldName "branch")

_TraversalMethod_by = (Core.FieldName "by")

_TraversalMethod_cap = (Core.FieldName "cap")

_TraversalMethod_choose = (Core.FieldName "choose")

_TraversalMethod_coalesce = (Core.FieldName "coalesce")

_TraversalMethod_coin = (Core.FieldName "coin")

_TraversalMethod_conjoin = (Core.FieldName "conjoin")

_TraversalMethod_connectedComponent = (Core.FieldName "connectedComponent")

_TraversalMethod_constant = (Core.FieldName "constant")

_TraversalMethod_count = (Core.FieldName "count")

_TraversalMethod_cyclicPath = (Core.FieldName "cyclicPath")

_TraversalMethod_dedup = (Core.FieldName "dedup")

_TraversalMethod_difference = (Core.FieldName "difference")

_TraversalMethod_disjunct = (Core.FieldName "disjunct")

_TraversalMethod_drop = (Core.FieldName "drop")

_TraversalMethod_elementMap = (Core.FieldName "elementMap")

_TraversalMethod_emit = (Core.FieldName "emit")

_TraversalMethod_filter = (Core.FieldName "filter")

_TraversalMethod_flatMap = (Core.FieldName "flatMap")

_TraversalMethod_fold = (Core.FieldName "fold")

_TraversalMethod_from = (Core.FieldName "from")

_TraversalMethod_group = (Core.FieldName "group")

_TraversalMethod_groupCount = (Core.FieldName "groupCount")

_TraversalMethod_has = (Core.FieldName "has")

_TraversalMethod_hasId = (Core.FieldName "hasId")

_TraversalMethod_hasKey = (Core.FieldName "hasKey")

_TraversalMethod_hasLabel = (Core.FieldName "hasLabel")

_TraversalMethod_hasNot = (Core.FieldName "hasNot")

_TraversalMethod_hasValue = (Core.FieldName "hasValue")

_TraversalMethod_id = (Core.FieldName "id")

_TraversalMethod_identity = (Core.FieldName "identity")

_TraversalMethod_in = (Core.FieldName "in")

_TraversalMethod_inE = (Core.FieldName "inE")

_TraversalMethod_intersect = (Core.FieldName "intersect")

_TraversalMethod_inV = (Core.FieldName "inV")

_TraversalMethod_index = (Core.FieldName "index")

_TraversalMethod_inject = (Core.FieldName "inject")

_TraversalMethod_is = (Core.FieldName "is")

_TraversalMethod_key = (Core.FieldName "key")

_TraversalMethod_label = (Core.FieldName "label")

_TraversalMethod_limit = (Core.FieldName "limit")

_TraversalMethod_local = (Core.FieldName "local")

_TraversalMethod_loops = (Core.FieldName "loops")

_TraversalMethod_map = (Core.FieldName "map")

_TraversalMethod_match = (Core.FieldName "match")

_TraversalMethod_math = (Core.FieldName "math")

_TraversalMethod_max = (Core.FieldName "max")

_TraversalMethod_mean = (Core.FieldName "mean")

_TraversalMethod_min = (Core.FieldName "min")

_TraversalMethod_none = (Core.FieldName "none")

_TraversalMethod_not = (Core.FieldName "not")

_TraversalMethod_option = (Core.FieldName "option")

_TraversalMethod_optional = (Core.FieldName "optional")

_TraversalMethod_or = (Core.FieldName "or")

_TraversalMethod_order = (Core.FieldName "order")

_TraversalMethod_otherV = (Core.FieldName "otherV")

_TraversalMethod_out = (Core.FieldName "out")

_TraversalMethod_outE = (Core.FieldName "outE")

_TraversalMethod_outV = (Core.FieldName "outV")

_TraversalMethod_pageRank = (Core.FieldName "pageRank")

_TraversalMethod_path = (Core.FieldName "path")

_TraversalMethod_peerPressure = (Core.FieldName "peerPressure")

_TraversalMethod_profile = (Core.FieldName "profile")

_TraversalMethod_project = (Core.FieldName "project")

_TraversalMethod_properties = (Core.FieldName "properties")

_TraversalMethod_property = (Core.FieldName "property")

_TraversalMethod_propertyMap = (Core.FieldName "propertyMap")

_TraversalMethod_range = (Core.FieldName "range")

_TraversalMethod_read = (Core.FieldName "read")

_TraversalMethod_repeat = (Core.FieldName "repeat")

_TraversalMethod_sack = (Core.FieldName "sack")

_TraversalMethod_sample = (Core.FieldName "sample")

_TraversalMethod_select = (Core.FieldName "select")

_TraversalMethod_combine = (Core.FieldName "combine")

_TraversalMethod_product = (Core.FieldName "product")

_TraversalMethod_merge = (Core.FieldName "merge")

_TraversalMethod_shortestPath = (Core.FieldName "shortestPath")

_TraversalMethod_sideEffect = (Core.FieldName "sideEffect")

_TraversalMethod_simplePath = (Core.FieldName "simplePath")

_TraversalMethod_skip = (Core.FieldName "skip")

_TraversalMethod_store = (Core.FieldName "store")

_TraversalMethod_subgraph = (Core.FieldName "subgraph")

_TraversalMethod_sum = (Core.FieldName "sum")

_TraversalMethod_tail = (Core.FieldName "tail")

_TraversalMethod_fail = (Core.FieldName "fail")

_TraversalMethod_times = (Core.FieldName "times")

_TraversalMethod_to = (Core.FieldName "to")

_TraversalMethod_toE = (Core.FieldName "toE")

_TraversalMethod_toV = (Core.FieldName "toV")

_TraversalMethod_tree = (Core.FieldName "tree")

_TraversalMethod_unfold = (Core.FieldName "unfold")

_TraversalMethod_union = (Core.FieldName "union")

_TraversalMethod_until = (Core.FieldName "until")

_TraversalMethod_value = (Core.FieldName "value")

_TraversalMethod_valueMap = (Core.FieldName "valueMap")

_TraversalMethod_values = (Core.FieldName "values")

_TraversalMethod_where = (Core.FieldName "where")

_TraversalMethod_with = (Core.FieldName "with")

_TraversalMethod_write = (Core.FieldName "write")

_TraversalMethod_element = (Core.FieldName "element")

_TraversalMethod_call = (Core.FieldName "call")

_TraversalMethod_concat = (Core.FieldName "concat")

_TraversalMethod_asString = (Core.FieldName "asString")

_TraversalMethod_format = (Core.FieldName "format")

_TraversalMethod_toUpper = (Core.FieldName "toUpper")

_TraversalMethod_toLower = (Core.FieldName "toLower")

_TraversalMethod_length = (Core.FieldName "length")

_TraversalMethod_trim = (Core.FieldName "trim")

_TraversalMethod_lTrim = (Core.FieldName "lTrim")

_TraversalMethod_rTrim = (Core.FieldName "rTrim")

_TraversalMethod_reverse = (Core.FieldName "reverse")

_TraversalMethod_replace = (Core.FieldName "replace")

_TraversalMethod_split = (Core.FieldName "split")

_TraversalMethod_substring = (Core.FieldName "substring")

_TraversalMethod_asDate = (Core.FieldName "asDate")

_TraversalMethod_dateAdd = (Core.FieldName "dateAdd")

_TraversalMethod_dateDiff = (Core.FieldName "dateDiff")

data StringArgumentOrNestedTraversal = 
  StringArgumentOrNestedTraversalString StringArgument |
  StringArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_StringArgumentOrNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentOrNestedTraversal")

_StringArgumentOrNestedTraversal_string = (Core.FieldName "string")

_StringArgumentOrNestedTraversal_traversal = (Core.FieldName "traversal")

data OptionalTraversalScopeArgumentAndStringArgument = 
  OptionalTraversalScopeArgumentAndStringArgument {
    optionalTraversalScopeArgumentAndStringArgumentScope :: (Maybe TraversalScopeArgument),
    optionalTraversalScopeArgumentAndStringArgumentString :: StringArgument}
  deriving (Eq, Ord, Read, Show)

_OptionalTraversalScopeArgumentAndStringArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndStringArgument")

_OptionalTraversalScopeArgumentAndStringArgument_scope = (Core.FieldName "scope")

_OptionalTraversalScopeArgumentAndStringArgument_string = (Core.FieldName "string")

data StringArgumentAndOptionalStringLiteralVarargs = 
  StringArgumentAndOptionalStringLiteralVarargs {
    stringArgumentAndOptionalStringLiteralVarargsFirst :: StringArgument,
    stringArgumentAndOptionalStringLiteralVarargsRest :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndOptionalStringLiteralVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgumentAndOptionalStringLiteralVarargs")

_StringArgumentAndOptionalStringLiteralVarargs_first = (Core.FieldName "first")

_StringArgumentAndOptionalStringLiteralVarargs_rest = (Core.FieldName "rest")

data TraversalSackMethodArgumentOrIntegerArgument = 
  TraversalSackMethodArgumentOrIntegerArgumentConsumer TraversalSackMethodArgument |
  TraversalSackMethodArgumentOrIntegerArgumentInt IntegerArgument
  deriving (Eq, Ord, Read, Show)

_TraversalSackMethodArgumentOrIntegerArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSackMethodArgumentOrIntegerArgument")

_TraversalSackMethodArgumentOrIntegerArgument_consumer = (Core.FieldName "consumer")

_TraversalSackMethodArgumentOrIntegerArgument_int = (Core.FieldName "int")

data ByArgs = 
  ByArgsOrder TraversalOrderArgument |
  ByArgsToken TraversalTokenArgument |
  ByArgsOther ByOtherArgs
  deriving (Eq, Ord, Read, Show)

_ByArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ByArgs")

_ByArgs_order = (Core.FieldName "order")

_ByArgs_token = (Core.FieldName "token")

_ByArgs_other = (Core.FieldName "other")

data ByOtherArgs = 
  ByOtherArgsComparator (Maybe TraversalComparatorArgument) |
  ByOtherArgsOther (Maybe TraversalFunctionArgumentOrStringArgumentOrNestedTraversal)
  deriving (Eq, Ord, Read, Show)

_ByOtherArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ByOtherArgs")

_ByOtherArgs_comparator = (Core.FieldName "comparator")

_ByOtherArgs_other = (Core.FieldName "other")

data TraversalFunctionArgumentOrStringArgumentOrNestedTraversal = 
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction TraversalFunctionArgument |
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalString StringArgument |
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_function = (Core.FieldName "function")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_string = (Core.FieldName "string")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_traversal = (Core.FieldName "traversal")

data ChooseArgs = 
  ChooseArgsFunction TraversalFunctionArgument |
  ChooseArgsPredicateTraversal PredicateTraversalArgument |
  ChooseArgsTraversal NestedTraversalArgument
  deriving (Eq, Ord, Read, Show)

_ChooseArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ChooseArgs")

_ChooseArgs_function = (Core.FieldName "function")

_ChooseArgs_predicateTraversal = (Core.FieldName "predicateTraversal")

_ChooseArgs_traversal = (Core.FieldName "traversal")

data PredicateTraversalArgument = 
  PredicateTraversalArgument {
    predicateTraversalArgumentPredicate :: TraversalPredicate,
    predicateTraversalArgumentTraversal1 :: NestedTraversal,
    predicateTraversalArgumentTraversal2 :: (Maybe NestedTraversal)}
  deriving (Eq, Ord, Read, Show)

_PredicateTraversalArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.PredicateTraversalArgument")

_PredicateTraversalArgument_predicate = (Core.FieldName "predicate")

_PredicateTraversalArgument_traversal1 = (Core.FieldName "traversal1")

_PredicateTraversalArgument_traversal2 = (Core.FieldName "traversal2")

data NestedTraversalArgument = 
  NestedTraversalArgument {
    nestedTraversalArgumentTraversal1 :: NestedTraversal,
    nestedTraversalArgumentTraversal2 :: (Maybe NestedTraversal),
    nestedTraversalArgumentTraversal3 :: (Maybe NestedTraversal)}
  deriving (Eq, Ord, Read, Show)

_NestedTraversalArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.NestedTraversalArgument")

_NestedTraversalArgument_traversal1 = (Core.FieldName "traversal1")

_NestedTraversalArgument_traversal2 = (Core.FieldName "traversal2")

_NestedTraversalArgument_traversal3 = (Core.FieldName "traversal3")

data DedupArgs = 
  DedupArgsScopeString ScopeStringArgument |
  DedupArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_DedupArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.DedupArgs")

_DedupArgs_scopeString = (Core.FieldName "scopeString")

_DedupArgs_string = (Core.FieldName "string")

data ScopeStringArgument = 
  ScopeStringArgument {
    scopeStringArgumentScope :: TraversalScopeArgument,
    scopeStringArgumentStrings :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_ScopeStringArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.ScopeStringArgument")

_ScopeStringArgument_scope = (Core.FieldName "scope")

_ScopeStringArgument_strings = (Core.FieldName "strings")

data PredicateOrTraversal = 
  PredicateOrTraversalPredicate TraversalPredicate |
  PredicateOrTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_PredicateOrTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.PredicateOrTraversal")

_PredicateOrTraversal_predicate = (Core.FieldName "predicate")

_PredicateOrTraversal_traversal = (Core.FieldName "traversal")

data GenericLiteralArgumentAndTraversalBiFunctionArgument = 
  GenericLiteralArgumentAndTraversalBiFunctionArgument {
    genericLiteralArgumentAndTraversalBiFunctionArgumentLiteral :: GenericLiteralArgument,
    genericLiteralArgumentAndTraversalBiFunctionArgumentBiFunction :: TraversalBiFunctionArgument}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndTraversalBiFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument")

_GenericLiteralArgumentAndTraversalBiFunctionArgument_literal = (Core.FieldName "literal")

_GenericLiteralArgumentAndTraversalBiFunctionArgument_biFunction = (Core.FieldName "biFunction")

data FromArgs = 
  FromArgsString StringArgument |
  FromArgsVertex StructureVertexArgument |
  FromArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_FromArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.FromArgs")

_FromArgs_string = (Core.FieldName "string")

_FromArgs_vertex = (Core.FieldName "vertex")

_FromArgs_traversal = (Core.FieldName "traversal")

data HasArgs = 
  HasArgsString HasStringArgumentAndOptionalStringLiteralVarargs |
  HasArgsTraversalToken HasTraversalTokenArgs
  deriving (Eq, Ord, Read, Show)

_HasArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.HasArgs")

_HasArgs_string = (Core.FieldName "string")

_HasArgs_traversalToken = (Core.FieldName "traversalToken")

data HasStringArgumentAndOptionalStringLiteralVarargs = 
  HasStringArgumentAndOptionalStringLiteralVarargs {
    hasStringArgumentAndOptionalStringLiteralVarargsString :: StringNullableArgument,
    hasStringArgumentAndOptionalStringLiteralVarargsRest :: (Maybe HasStringArgumentAndOptionalStringLiteralVarargsRest)}
  deriving (Eq, Ord, Read, Show)

_HasStringArgumentAndOptionalStringLiteralVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargs")

_HasStringArgumentAndOptionalStringLiteralVarargs_string = (Core.FieldName "string")

_HasStringArgumentAndOptionalStringLiteralVarargs_rest = (Core.FieldName "rest")

data HasStringArgumentAndOptionalStringLiteralVarargsRest = 
  HasStringArgumentAndOptionalStringLiteralVarargsRestObject GenericLiteralArgument |
  HasStringArgumentAndOptionalStringLiteralVarargsRestPredicate TraversalPredicate |
  HasStringArgumentAndOptionalStringLiteralVarargsRestStringObject StringNullableArgumentAndGenericLiteralArgument |
  HasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate StringNullableArgumentAndTraversalPredicate |
  HasStringArgumentAndOptionalStringLiteralVarargsRestTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_HasStringArgumentAndOptionalStringLiteralVarargsRest = (Core.Name "hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_object = (Core.FieldName "object")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_predicate = (Core.FieldName "predicate")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_stringObject = (Core.FieldName "stringObject")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_stringPredicate = (Core.FieldName "stringPredicate")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_traversal = (Core.FieldName "traversal")

data StringNullableArgumentAndGenericLiteralArgument = 
  StringNullableArgumentAndGenericLiteralArgument {
    stringNullableArgumentAndGenericLiteralArgumentString :: StringNullableArgument,
    stringNullableArgumentAndGenericLiteralArgumentLiteral :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_StringNullableArgumentAndGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndGenericLiteralArgument")

_StringNullableArgumentAndGenericLiteralArgument_string = (Core.FieldName "string")

_StringNullableArgumentAndGenericLiteralArgument_literal = (Core.FieldName "literal")

data StringNullableArgumentAndTraversalPredicate = 
  StringNullableArgumentAndTraversalPredicate {
    stringNullableArgumentAndTraversalPredicateString :: StringNullableArgument,
    stringNullableArgumentAndTraversalPredicatePredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_StringNullableArgumentAndTraversalPredicate = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgumentAndTraversalPredicate")

_StringNullableArgumentAndTraversalPredicate_string = (Core.FieldName "string")

_StringNullableArgumentAndTraversalPredicate_predicate = (Core.FieldName "predicate")

data HasTraversalTokenArgs = 
  HasTraversalTokenArgs {
    hasTraversalTokenArgsTraversalToken :: TraversalTokenArgument,
    hasTraversalTokenArgsRest :: HasTraversalTokenArgsRest}
  deriving (Eq, Ord, Read, Show)

_HasTraversalTokenArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.HasTraversalTokenArgs")

_HasTraversalTokenArgs_traversalToken = (Core.FieldName "traversalToken")

_HasTraversalTokenArgs_rest = (Core.FieldName "rest")

data HasTraversalTokenArgsRest = 
  HasTraversalTokenArgsRestLiteral GenericLiteralArgument |
  HasTraversalTokenArgsRestPredicate TraversalPredicate |
  HasTraversalTokenArgsRestTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_HasTraversalTokenArgsRest = (Core.Name "hydra/langs/tinkerpop/gremlin.HasTraversalTokenArgsRest")

_HasTraversalTokenArgsRest_literal = (Core.FieldName "literal")

_HasTraversalTokenArgsRest_predicate = (Core.FieldName "predicate")

_HasTraversalTokenArgsRest_traversal = (Core.FieldName "traversal")

data GenericLiteralArgumentAndTraversalPredicate = 
  GenericLiteralArgumentAndTraversalPredicateLiteral GenericLiteralArgument |
  GenericLiteralArgumentAndTraversalPredicatePredicate TraversalPredicate
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndTraversalPredicate = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndTraversalPredicate")

_GenericLiteralArgumentAndTraversalPredicate_literal = (Core.FieldName "literal")

_GenericLiteralArgumentAndTraversalPredicate_predicate = (Core.FieldName "predicate")

data TraversalPredicateOrStringLiteralVarargs = 
  TraversalPredicateOrStringLiteralVarargsPredicate TraversalPredicate |
  TraversalPredicateOrStringLiteralVarargsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateOrStringLiteralVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateOrStringLiteralVarargs")

_TraversalPredicateOrStringLiteralVarargs_predicate = (Core.FieldName "predicate")

_TraversalPredicateOrStringLiteralVarargs_string = (Core.FieldName "string")

data TraversalPredicateOrGenericLiteralArgument = 
  TraversalPredicateOrGenericLiteralArgumentPredicate TraversalPredicate |
  TraversalPredicateOrGenericLiteralArgumentLiteral [GenericLiteralArgument]
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateOrGenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateOrGenericLiteralArgument")

_TraversalPredicateOrGenericLiteralArgument_predicate = (Core.FieldName "predicate")

_TraversalPredicateOrGenericLiteralArgument_literal = (Core.FieldName "literal")

data OptionArgs = 
  OptionArgsPredicateTraversal TraversalPredicateAndNestedTraversal |
  OptionArgsMergeMap TraversalMergeArgumentAndGenericLiteralMapNullableArgument |
  OptionArgsMergeTraversal TraversalMergeArgumentAndNestedTraversal |
  OptionArgsObjectTraversal GenericLiteralArgumentAndNestedTraversal |
  OptionArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_OptionArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionArgs")

_OptionArgs_predicateTraversal = (Core.FieldName "predicateTraversal")

_OptionArgs_mergeMap = (Core.FieldName "mergeMap")

_OptionArgs_mergeTraversal = (Core.FieldName "mergeTraversal")

_OptionArgs_objectTraversal = (Core.FieldName "objectTraversal")

_OptionArgs_traversal = (Core.FieldName "traversal")

data TraversalPredicateAndNestedTraversal = 
  TraversalPredicateAndNestedTraversal {
    traversalPredicateAndNestedTraversalPredicate :: TraversalPredicate,
    traversalPredicateAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPredicateAndNestedTraversal")

_TraversalPredicateAndNestedTraversal_predicate = (Core.FieldName "predicate")

_TraversalPredicateAndNestedTraversal_traversal = (Core.FieldName "traversal")

data TraversalMergeArgumentAndGenericLiteralMapNullableArgument = 
  TraversalMergeArgumentAndGenericLiteralMapNullableArgument {
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentMerge :: TraversalMergeArgument,
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentMap :: GenericLiteralMapNullableArgument,
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentCardinality :: (Maybe TraversalCardinality)}
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_merge = (Core.FieldName "merge")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_map = (Core.FieldName "map")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_cardinality = (Core.FieldName "cardinality")

data TraversalMergeArgumentAndNestedTraversal = 
  TraversalMergeArgumentAndNestedTraversal {
    traversalMergeArgumentAndNestedTraversalMerge :: TraversalMergeArgument,
    traversalMergeArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgumentAndNestedTraversal")

_TraversalMergeArgumentAndNestedTraversal_merge = (Core.FieldName "merge")

_TraversalMergeArgumentAndNestedTraversal_traversal = (Core.FieldName "traversal")

data GenericLiteralArgumentAndNestedTraversal = 
  GenericLiteralArgumentAndNestedTraversal {
    genericLiteralArgumentAndNestedTraversalObject :: GenericLiteralArgument,
    genericLiteralArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndNestedTraversal")

_GenericLiteralArgumentAndNestedTraversal_object = (Core.FieldName "object")

_GenericLiteralArgumentAndNestedTraversal_traversal = (Core.FieldName "traversal")

data PropertyArgs = 
  PropertyArgsCardinalityObjects TraversalCardinalityArgumentAndObjects |
  PropertyArgsObjects [GenericLiteralArgument] |
  PropertyArgsObject GenericLiteralMapNullableArgument |
  PropertyArgsCardinalityObject GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
  deriving (Eq, Ord, Read, Show)

_PropertyArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.PropertyArgs")

_PropertyArgs_cardinalityObjects = (Core.FieldName "cardinalityObjects")

_PropertyArgs_objects = (Core.FieldName "objects")

_PropertyArgs_object = (Core.FieldName "object")

_PropertyArgs_cardinalityObject = (Core.FieldName "cardinalityObject")

data TraversalCardinalityArgumentAndObjects = 
  TraversalCardinalityArgumentAndObjects {
    traversalCardinalityArgumentAndObjectsCardinality :: TraversalCardinalityArgument,
    traversalCardinalityArgumentAndObjectsObjects :: [GenericLiteralArgument]}
  deriving (Eq, Ord, Read, Show)

_TraversalCardinalityArgumentAndObjects = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinalityArgumentAndObjects")

_TraversalCardinalityArgumentAndObjects_cardinality = (Core.FieldName "cardinality")

_TraversalCardinalityArgumentAndObjects_objects = (Core.FieldName "objects")

data GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument = 
  GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument {
    genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentCardinality :: TraversalCardinalityArgument,
    genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentObject :: GenericLiteralMapNullableArgument}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument")

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_cardinality = (Core.FieldName "cardinality")

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_object = (Core.FieldName "object")

data RangeArgs = 
  RangeArgs {
    rangeArgsScope :: (Maybe TraversalScopeArgument),
    rangeArgsMin :: IntegerArgument,
    rangeArgsMax :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_RangeArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.RangeArgs")

_RangeArgs_scope = (Core.FieldName "scope")

_RangeArgs_min = (Core.FieldName "min")

_RangeArgs_max = (Core.FieldName "max")

data OptionalStringArgumentAndNestedTraversal = 
  OptionalStringArgumentAndNestedTraversal {
    optionalStringArgumentAndNestedTraversalString :: (Maybe StringArgument),
    optionalStringArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_OptionalStringArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalStringArgumentAndNestedTraversal")

_OptionalStringArgumentAndNestedTraversal_string = (Core.FieldName "string")

_OptionalStringArgumentAndNestedTraversal_traversal = (Core.FieldName "traversal")

data SelectArgs = 
  SelectArgsColumn TraversalColumnArgument |
  SelectArgsPopStrings PopStringsArgument |
  SelectArgsPopTraversal TraversalPopArgumentAndNestedTraversal |
  SelectArgsStrings [StringArgument] |
  SelectArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_SelectArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.SelectArgs")

_SelectArgs_column = (Core.FieldName "column")

_SelectArgs_popStrings = (Core.FieldName "popStrings")

_SelectArgs_popTraversal = (Core.FieldName "popTraversal")

_SelectArgs_strings = (Core.FieldName "strings")

_SelectArgs_traversal = (Core.FieldName "traversal")

data PopStringsArgument = 
  PopStringsArgument {
    popStringsArgumentPop :: TraversalPopArgument,
    popStringsArgumentString :: [StringArgument]}
  deriving (Eq, Ord, Read, Show)

_PopStringsArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.PopStringsArgument")

_PopStringsArgument_pop = (Core.FieldName "pop")

_PopStringsArgument_string = (Core.FieldName "string")

data TraversalPopArgumentAndNestedTraversal = 
  TraversalPopArgumentAndNestedTraversal {
    traversalPopArgumentAndNestedTraversalPop :: TraversalPopArgument,
    traversalPopArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalPopArgumentAndNestedTraversal = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPopArgumentAndNestedTraversal")

_TraversalPopArgumentAndNestedTraversal_pop = (Core.FieldName "pop")

_TraversalPopArgumentAndNestedTraversal_traversal = (Core.FieldName "traversal")

data OptionalTraversalScopeArgumentAndIntegerArgument = 
  OptionalTraversalScopeArgumentAndIntegerArgument {
    optionalTraversalScopeArgumentAndIntegerArgumentScope :: (Maybe TraversalScopeArgument),
    optionalTraversalScopeArgumentAndIntegerArgumentLong :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_OptionalTraversalScopeArgumentAndIntegerArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.OptionalTraversalScopeArgumentAndIntegerArgument")

_OptionalTraversalScopeArgumentAndIntegerArgument_scope = (Core.FieldName "scope")

_OptionalTraversalScopeArgumentAndIntegerArgument_long = (Core.FieldName "long")

data TailArgs = 
  TailArgs {
    tailArgsScope :: (Maybe TraversalScopeArgument),
    tailArgsInteger :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)

_TailArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.TailArgs")

_TailArgs_scope = (Core.FieldName "scope")

_TailArgs_integer = (Core.FieldName "integer")

data ToArgs = 
  ToArgsDirection DirectionAndVarargs |
  ToArgsString StringArgument |
  ToArgsVertex StructureVertexArgument |
  ToArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_ToArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ToArgs")

_ToArgs_direction = (Core.FieldName "direction")

_ToArgs_string = (Core.FieldName "string")

_ToArgs_vertex = (Core.FieldName "vertex")

_ToArgs_traversal = (Core.FieldName "traversal")

data DirectionAndVarargs = 
  DirectionAndVarargs {
    directionAndVarargsDirection :: TraversalDirectionArgument,
    directionAndVarargsVarargs :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_DirectionAndVarargs = (Core.Name "hydra/langs/tinkerpop/gremlin.DirectionAndVarargs")

_DirectionAndVarargs_direction = (Core.FieldName "direction")

_DirectionAndVarargs_varargs = (Core.FieldName "varargs")

data ValueMapArgs = 
  ValueMapArgsString [StringNullableArgument] |
  ValueMapArgsBoolean ValueMapBooleanArgs
  deriving (Eq, Ord, Read, Show)

_ValueMapArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ValueMapArgs")

_ValueMapArgs_string = (Core.FieldName "string")

_ValueMapArgs_boolean = (Core.FieldName "boolean")

data ValueMapBooleanArgs = 
  ValueMapBooleanArgs {
    valueMapBooleanArgsValue :: BooleanArgument,
    valueMapBooleanArgsKeys :: (Maybe [StringNullableArgument])}
  deriving (Eq, Ord, Read, Show)

_ValueMapBooleanArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ValueMapBooleanArgs")

_ValueMapBooleanArgs_value = (Core.FieldName "value")

_ValueMapBooleanArgs_keys = (Core.FieldName "keys")

data WhereArgs = 
  WhereArgsPredicate WhereWithPredicateArgs |
  WhereArgsString StringArgument |
  WhereArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_WhereArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.WhereArgs")

_WhereArgs_predicate = (Core.FieldName "predicate")

_WhereArgs_string = (Core.FieldName "string")

_WhereArgs_traversal = (Core.FieldName "traversal")

data WhereWithPredicateArgs = 
  WhereWithPredicateArgs {
    whereWithPredicateArgsLeftArg :: (Maybe StringArgument),
    whereWithPredicateArgsPredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_WhereWithPredicateArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.WhereWithPredicateArgs")

_WhereWithPredicateArgs_leftArg = (Core.FieldName "leftArg")

_WhereWithPredicateArgs_predicate = (Core.FieldName "predicate")

data WithArgs = 
  WithArgs {
    withArgsKeys :: WithArgsKeys,
    withArgsValues :: (Maybe WithArgsValues)}
  deriving (Eq, Ord, Read, Show)

_WithArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgs")

_WithArgs_keys = (Core.FieldName "keys")

_WithArgs_values = (Core.FieldName "values")

data WithArgsKeys = 
  WithArgsKeysWithOption WithOptionKeys |
  WithArgsKeysString StringArgument
  deriving (Eq, Ord, Read, Show)

_WithArgsKeys = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgsKeys")

_WithArgsKeys_withOption = (Core.FieldName "withOption")

_WithArgsKeys_string = (Core.FieldName "string")

data WithArgsValues = 
  WithArgsValuesWithOptions WithOptionsValues |
  WithArgsValuesIo IoOptionsValues |
  WithArgsValuesObject GenericLiteralArgument
  deriving (Eq, Ord, Read, Show)

_WithArgsValues = (Core.Name "hydra/langs/tinkerpop/gremlin.WithArgsValues")

_WithArgsValues_withOptions = (Core.FieldName "withOptions")

_WithArgsValues_io = (Core.FieldName "io")

_WithArgsValues_object = (Core.FieldName "object")

data ConcatArgs = 
  ConcatArgsTraversal [NestedTraversal] |
  ConcatArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_ConcatArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ConcatArgs")

_ConcatArgs_traversal = (Core.FieldName "traversal")

_ConcatArgs_string = (Core.FieldName "string")

data ReplaceArgs = 
  ReplaceArgs {
    replaceArgsScope :: (Maybe TraversalScopeArgument),
    replaceArgsFrom :: StringNullableArgument,
    replaceArgsTo :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)

_ReplaceArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.ReplaceArgs")

_ReplaceArgs_scope = (Core.FieldName "scope")

_ReplaceArgs_from = (Core.FieldName "from")

_ReplaceArgs_to = (Core.FieldName "to")

data SplitArgs = 
  SplitArgs {
    splitArgsScope :: (Maybe TraversalScopeArgument),
    splitArgsDelimiter :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)

_SplitArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.SplitArgs")

_SplitArgs_scope = (Core.FieldName "scope")

_SplitArgs_delimiter = (Core.FieldName "delimiter")

data SubstringArgs = 
  SubstringArgs {
    substringArgsScope :: (Maybe TraversalScopeArgument),
    substringArgsStart :: IntegerArgument,
    substringArgsEnd :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)

_SubstringArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.SubstringArgs")

_SubstringArgs_scope = (Core.FieldName "scope")

_SubstringArgs_start = (Core.FieldName "start")

_SubstringArgs_end = (Core.FieldName "end")

data DateAddArgs = 
  DateAddArgs {
    dateAddArgsUnit :: TraversalDTArgument,
    dateAddArgsDuration :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_DateAddArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.DateAddArgs")

_DateAddArgs_unit = (Core.FieldName "unit")

_DateAddArgs_duration = (Core.FieldName "duration")

data DateDiffArgs = 
  DateDiffArgsTraversal NestedTraversal |
  DateDiffArgsDate DateArgument
  deriving (Eq, Ord, Read, Show)

_DateDiffArgs = (Core.Name "hydra/langs/tinkerpop/gremlin.DateDiffArgs")

_DateDiffArgs_traversal = (Core.FieldName "traversal")

_DateDiffArgs_date = (Core.FieldName "date")

data StructureVertex = 
  StructureVertex {
    structureVertexNew :: Bool,
    structureVertexId :: GenericLiteralArgument,
    structureVertexLabel :: StringArgument}
  deriving (Eq, Ord, Read, Show)

_StructureVertex = (Core.Name "hydra/langs/tinkerpop/gremlin.StructureVertex")

_StructureVertex_new = (Core.FieldName "new")

_StructureVertex_id = (Core.FieldName "id")

_StructureVertex_label = (Core.FieldName "label")

data TraversalStrategy = 
  TraversalStrategy {
    traversalStrategyNew :: Bool,
    traversalStrategyClass :: Identifier,
    traversalStrategyConfigurations :: [Configuration]}
  deriving (Eq, Ord, Read, Show)

_TraversalStrategy = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalStrategy")

_TraversalStrategy_new = (Core.FieldName "new")

_TraversalStrategy_class = (Core.FieldName "class")

_TraversalStrategy_configurations = (Core.FieldName "configurations")

data Configuration = 
  Configuration {
    configurationKey :: KeywordOrIdentifier,
    configurationValue :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_Configuration = (Core.Name "hydra/langs/tinkerpop/gremlin.Configuration")

_Configuration_key = (Core.FieldName "key")

_Configuration_value = (Core.FieldName "value")

data KeywordOrIdentifier = 
  KeywordOrIdentifierKeyword Keyword |
  KeywordOrIdentifierIdentifier Identifier
  deriving (Eq, Ord, Read, Show)

_KeywordOrIdentifier = (Core.Name "hydra/langs/tinkerpop/gremlin.KeywordOrIdentifier")

_KeywordOrIdentifier_keyword = (Core.FieldName "keyword")

_KeywordOrIdentifier_identifier = (Core.FieldName "identifier")

data TraversalScope = 
  TraversalScopeLocal  |
  TraversalScopeGlobal 
  deriving (Eq, Ord, Read, Show)

_TraversalScope = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalScope")

_TraversalScope_local = (Core.FieldName "local")

_TraversalScope_global = (Core.FieldName "global")

data TraversalToken = 
  TraversalTokenId  |
  TraversalTokenLabel  |
  TraversalTokenKey  |
  TraversalTokenValue 
  deriving (Eq, Ord, Read, Show)

_TraversalToken = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalToken")

_TraversalToken_id = (Core.FieldName "id")

_TraversalToken_label = (Core.FieldName "label")

_TraversalToken_key = (Core.FieldName "key")

_TraversalToken_value = (Core.FieldName "value")

data TraversalMerge = 
  TraversalMergeOnCreate  |
  TraversalMergeOnMatch  |
  TraversalMergeOutV  |
  TraversalMergeInV 
  deriving (Eq, Ord, Read, Show)

_TraversalMerge = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMerge")

_TraversalMerge_onCreate = (Core.FieldName "onCreate")

_TraversalMerge_onMatch = (Core.FieldName "onMatch")

_TraversalMerge_outV = (Core.FieldName "outV")

_TraversalMerge_inV = (Core.FieldName "inV")

data TraversalOrder = 
  TraversalOrderIncr  |
  TraversalOrderDecr  |
  TraversalOrderAsc  |
  TraversalOrderDesc  |
  TraversalOrderShuffle 
  deriving (Eq, Ord, Read, Show)

_TraversalOrder = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOrder")

_TraversalOrder_incr = (Core.FieldName "incr")

_TraversalOrder_decr = (Core.FieldName "decr")

_TraversalOrder_asc = (Core.FieldName "asc")

_TraversalOrder_desc = (Core.FieldName "desc")

_TraversalOrder_shuffle = (Core.FieldName "shuffle")

data TraversalDirection = 
  TraversalDirectionIn  |
  TraversalDirectionOut  |
  TraversalDirectionBoth 
  deriving (Eq, Ord, Read, Show)

_TraversalDirection = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDirection")

_TraversalDirection_in = (Core.FieldName "in")

_TraversalDirection_out = (Core.FieldName "out")

_TraversalDirection_both = (Core.FieldName "both")

data TraversalCardinality = 
  TraversalCardinalitySingle GenericLiteral |
  TraversalCardinalitySet GenericLiteral |
  TraversalCardinalityList GenericLiteral
  deriving (Eq, Ord, Read, Show)

_TraversalCardinality = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinality")

_TraversalCardinality_single = (Core.FieldName "single")

_TraversalCardinality_set = (Core.FieldName "set")

_TraversalCardinality_list = (Core.FieldName "list")

data TraversalColumn = 
  TraversalColumnKeys  |
  TraversalColumnValues 
  deriving (Eq, Ord, Read, Show)

_TraversalColumn = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalColumn")

_TraversalColumn_keys = (Core.FieldName "keys")

_TraversalColumn_values = (Core.FieldName "values")

data TraversalPop = 
  TraversalPopFirst  |
  TraversalPopLast  |
  TraversalPopAll  |
  TraversalPopMixed 
  deriving (Eq, Ord, Read, Show)

_TraversalPop = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPop")

_TraversalPop_first = (Core.FieldName "first")

_TraversalPop_last = (Core.FieldName "last")

_TraversalPop_all = (Core.FieldName "all")

_TraversalPop_mixed = (Core.FieldName "mixed")

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

_TraversalOperator_addAll = (Core.FieldName "addAll")

_TraversalOperator_and = (Core.FieldName "and")

_TraversalOperator_assign = (Core.FieldName "assign")

_TraversalOperator_div = (Core.FieldName "div")

_TraversalOperator_max = (Core.FieldName "max")

_TraversalOperator_min = (Core.FieldName "min")

_TraversalOperator_minus = (Core.FieldName "minus")

_TraversalOperator_mult = (Core.FieldName "mult")

_TraversalOperator_or = (Core.FieldName "or")

_TraversalOperator_sum = (Core.FieldName "sum")

_TraversalOperator_sumLong = (Core.FieldName "sumLong")

data TraversalPick = 
  TraversalPickAny  |
  TraversalPickNone 
  deriving (Eq, Ord, Read, Show)

_TraversalPick = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPick")

_TraversalPick_any = (Core.FieldName "any")

_TraversalPick_none = (Core.FieldName "none")

data TraversalDT = 
  TraversalDTSecond  |
  TraversalDTMinute  |
  TraversalDTHour  |
  TraversalDTDay 
  deriving (Eq, Ord, Read, Show)

_TraversalDT = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDT")

_TraversalDT_second = (Core.FieldName "second")

_TraversalDT_minute = (Core.FieldName "minute")

_TraversalDT_hour = (Core.FieldName "hour")

_TraversalDT_day = (Core.FieldName "day")

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

_TraversalPredicate_eq = (Core.FieldName "eq")

_TraversalPredicate_neq = (Core.FieldName "neq")

_TraversalPredicate_lt = (Core.FieldName "lt")

_TraversalPredicate_lte = (Core.FieldName "lte")

_TraversalPredicate_gt = (Core.FieldName "gt")

_TraversalPredicate_gte = (Core.FieldName "gte")

_TraversalPredicate_inside = (Core.FieldName "inside")

_TraversalPredicate_outside = (Core.FieldName "outside")

_TraversalPredicate_between = (Core.FieldName "between")

_TraversalPredicate_within = (Core.FieldName "within")

_TraversalPredicate_without = (Core.FieldName "without")

_TraversalPredicate_not = (Core.FieldName "not")

_TraversalPredicate_startingWith = (Core.FieldName "startingWith")

_TraversalPredicate_notStartingWith = (Core.FieldName "notStartingWith")

_TraversalPredicate_endingWith = (Core.FieldName "endingWith")

_TraversalPredicate_notEndingWith = (Core.FieldName "notEndingWith")

_TraversalPredicate_containing = (Core.FieldName "containing")

_TraversalPredicate_notContaining = (Core.FieldName "notContaining")

_TraversalPredicate_regex = (Core.FieldName "regex")

_TraversalPredicate_notRegex = (Core.FieldName "notRegex")

_TraversalPredicate_and = (Core.FieldName "and")

_TraversalPredicate_or = (Core.FieldName "or")

_TraversalPredicate_negate = (Core.FieldName "negate")

data TwoTraversalPredicates = 
  TwoTraversalPredicates {
    twoTraversalPredicatesLeft :: TraversalPredicate,
    twoTraversalPredicatesRight :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_TwoTraversalPredicates = (Core.Name "hydra/langs/tinkerpop/gremlin.TwoTraversalPredicates")

_TwoTraversalPredicates_left = (Core.FieldName "left")

_TwoTraversalPredicates_right = (Core.FieldName "right")

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

_TraversalTerminalMethod_explain = (Core.FieldName "explain")

_TraversalTerminalMethod_iterate = (Core.FieldName "iterate")

_TraversalTerminalMethod_hasNext = (Core.FieldName "hasNext")

_TraversalTerminalMethod_tryNext = (Core.FieldName "tryNext")

_TraversalTerminalMethod_next = (Core.FieldName "next")

_TraversalTerminalMethod_toList = (Core.FieldName "toList")

_TraversalTerminalMethod_toSet = (Core.FieldName "toSet")

_TraversalTerminalMethod_toBulkSet = (Core.FieldName "toBulkSet")

data TraversalSelfMethod = 
  TraversalSelfMethodDiscard 
  deriving (Eq, Ord, Read, Show)

_TraversalSelfMethod = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSelfMethod")

_TraversalSelfMethod_discard = (Core.FieldName "discard")

data TraversalFunction = 
  TraversalFunctionToken TraversalToken |
  TraversalFunctionColumn TraversalColumn
  deriving (Eq, Ord, Read, Show)

_TraversalFunction = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunction")

_TraversalFunction_token = (Core.FieldName "token")

_TraversalFunction_column = (Core.FieldName "column")

data RangeArgument = 
  RangeArgument {
    rangeArgumentMin :: GenericLiteralArgument,
    rangeArgumentMax :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_RangeArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.RangeArgument")

_RangeArgument_min = (Core.FieldName "min")

_RangeArgument_max = (Core.FieldName "max")

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

_WithOptionKeys_shortestPath = (Core.FieldName "shortestPath")

_WithOptionKeys_connectedComponent = (Core.FieldName "connectedComponent")

_WithOptionKeys_pageRank = (Core.FieldName "pageRank")

_WithOptionKeys_peerPressure = (Core.FieldName "peerPressure")

_WithOptionKeys_io = (Core.FieldName "io")

_WithOptionKeys_withOptionsTokens = (Core.FieldName "withOptionsTokens")

_WithOptionKeys_withOptionsIndexer = (Core.FieldName "withOptionsIndexer")

data ConnectedComponentConstants = 
  ConnectedComponentConstantsComponent  |
  ConnectedComponentConstantsEdges  |
  ConnectedComponentConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_ConnectedComponentConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.ConnectedComponentConstants")

_ConnectedComponentConstants_component = (Core.FieldName "component")

_ConnectedComponentConstants_edges = (Core.FieldName "edges")

_ConnectedComponentConstants_propertyName = (Core.FieldName "propertyName")

data PageRankConstants = 
  PageRankConstantsEdges  |
  PageRankConstantsTimes  |
  PageRankConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_PageRankConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.PageRankConstants")

_PageRankConstants_edges = (Core.FieldName "edges")

_PageRankConstants_times = (Core.FieldName "times")

_PageRankConstants_propertyName = (Core.FieldName "propertyName")

data PeerPressureConstants = 
  PeerPressureConstantsEdges  |
  PeerPressureConstantsTimes  |
  PeerPressureConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_PeerPressureConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.PeerPressureConstants")

_PeerPressureConstants_edges = (Core.FieldName "edges")

_PeerPressureConstants_times = (Core.FieldName "times")

_PeerPressureConstants_propertyName = (Core.FieldName "propertyName")

data ShortestPathConstants = 
  ShortestPathConstantsTarget  |
  ShortestPathConstantsEdges  |
  ShortestPathConstantsDistance  |
  ShortestPathConstantsMaxDistance  |
  ShortestPathConstantsIncludeEdges 
  deriving (Eq, Ord, Read, Show)

_ShortestPathConstants = (Core.Name "hydra/langs/tinkerpop/gremlin.ShortestPathConstants")

_ShortestPathConstants_target = (Core.FieldName "target")

_ShortestPathConstants_edges = (Core.FieldName "edges")

_ShortestPathConstants_distance = (Core.FieldName "distance")

_ShortestPathConstants_maxDistance = (Core.FieldName "maxDistance")

_ShortestPathConstants_includeEdges = (Core.FieldName "includeEdges")

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

_WithOptionsValues_tokens = (Core.FieldName "tokens")

_WithOptionsValues_none = (Core.FieldName "none")

_WithOptionsValues_ids = (Core.FieldName "ids")

_WithOptionsValues_labels = (Core.FieldName "labels")

_WithOptionsValues_keys = (Core.FieldName "keys")

_WithOptionsValues_values = (Core.FieldName "values")

_WithOptionsValues_all = (Core.FieldName "all")

_WithOptionsValues_list = (Core.FieldName "list")

_WithOptionsValues_map = (Core.FieldName "map")

data IoOptionsKeys = 
  IoOptionsKeysReader  |
  IoOptionsKeysWriter 
  deriving (Eq, Ord, Read, Show)

_IoOptionsKeys = (Core.Name "hydra/langs/tinkerpop/gremlin.IoOptionsKeys")

_IoOptionsKeys_reader = (Core.FieldName "reader")

_IoOptionsKeys_writer = (Core.FieldName "writer")

data IoOptionsValues = 
  IoOptionsValuesGryo  |
  IoOptionsValuesGraphson  |
  IoOptionsValuesGraphml 
  deriving (Eq, Ord, Read, Show)

_IoOptionsValues = (Core.Name "hydra/langs/tinkerpop/gremlin.IoOptionsValues")

_IoOptionsValues_gryo = (Core.FieldName "gryo")

_IoOptionsValues_graphson = (Core.FieldName "graphson")

_IoOptionsValues_graphml = (Core.FieldName "graphml")

data BooleanArgument = 
  BooleanArgumentValue Bool |
  BooleanArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_BooleanArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.BooleanArgument")

_BooleanArgument_value = (Core.FieldName "value")

_BooleanArgument_variable = (Core.FieldName "variable")

data IntegerArgument = 
  IntegerArgumentValue IntegerLiteral |
  IntegerArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_IntegerArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerArgument")

_IntegerArgument_value = (Core.FieldName "value")

_IntegerArgument_variable = (Core.FieldName "variable")

data FloatArgument = 
  FloatArgumentValue FloatLiteral |
  FloatArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_FloatArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.FloatArgument")

_FloatArgument_value = (Core.FieldName "value")

_FloatArgument_variable = (Core.FieldName "variable")

data StringArgument = 
  StringArgumentValue String |
  StringArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StringArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringArgument")

_StringArgument_value = (Core.FieldName "value")

_StringArgument_variable = (Core.FieldName "variable")

data StringNullableArgument = 
  StringNullableArgumentValue (Maybe String) |
  StringNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StringNullableArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StringNullableArgument")

_StringNullableArgument_value = (Core.FieldName "value")

_StringNullableArgument_variable = (Core.FieldName "variable")

data DateArgument = 
  DateArgumentValue DateLiteral |
  DateArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_DateArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.DateArgument")

_DateArgument_value = (Core.FieldName "value")

_DateArgument_variable = (Core.FieldName "variable")

data GenericLiteralArgument = 
  GenericLiteralArgumentValue GenericLiteral |
  GenericLiteralArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralArgument")

_GenericLiteralArgument_value = (Core.FieldName "value")

_GenericLiteralArgument_variable = (Core.FieldName "variable")

data GenericLiteralListArgument = 
  GenericLiteralListArgumentValue GenericLiteralList |
  GenericLiteralListArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralListArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralListArgument")

_GenericLiteralListArgument_value = (Core.FieldName "value")

_GenericLiteralListArgument_variable = (Core.FieldName "variable")

data GenericLiteralMapArgument = 
  GenericLiteralMapArgumentValue GenericLiteralMap |
  GenericLiteralMapArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapArgument")

_GenericLiteralMapArgument_value = (Core.FieldName "value")

_GenericLiteralMapArgument_variable = (Core.FieldName "variable")

data GenericLiteralMapNullableArgument = 
  GenericLiteralMapNullableArgumentValue (Maybe GenericLiteralMap) |
  GenericLiteralMapNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMapNullableArgument")

_GenericLiteralMapNullableArgument_value = (Core.FieldName "value")

_GenericLiteralMapNullableArgument_variable = (Core.FieldName "variable")

data StructureVertexArgument = 
  StructureVertexArgumentValue StructureVertex |
  StructureVertexArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StructureVertexArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.StructureVertexArgument")

_StructureVertexArgument_value = (Core.FieldName "value")

_StructureVertexArgument_variable = (Core.FieldName "variable")

data TraversalCardinalityArgument = 
  TraversalCardinalityArgumentValue TraversalCardinality |
  TraversalCardinalityArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalCardinalityArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalCardinalityArgument")

_TraversalCardinalityArgument_value = (Core.FieldName "value")

_TraversalCardinalityArgument_variable = (Core.FieldName "variable")

data TraversalColumnArgument = 
  TraversalColumnArgumentValue TraversalColumn |
  TraversalColumnArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalColumnArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalColumnArgument")

_TraversalColumnArgument_value = (Core.FieldName "value")

_TraversalColumnArgument_variable = (Core.FieldName "variable")

data TraversalDirectionArgument = 
  TraversalDirectionArgumentValue TraversalDirection |
  TraversalDirectionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalDirectionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDirectionArgument")

_TraversalDirectionArgument_value = (Core.FieldName "value")

_TraversalDirectionArgument_variable = (Core.FieldName "variable")

data TraversalMergeArgument = 
  TraversalMergeArgumentValue TraversalMerge |
  TraversalMergeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalMergeArgument")

_TraversalMergeArgument_value = (Core.FieldName "value")

_TraversalMergeArgument_variable = (Core.FieldName "variable")

data TraversalOrderArgument = 
  TraversalOrderArgumentValue TraversalOrder |
  TraversalOrderArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalOrderArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalOrderArgument")

_TraversalOrderArgument_value = (Core.FieldName "value")

_TraversalOrderArgument_variable = (Core.FieldName "variable")

data TraversalPopArgument = 
  TraversalPopArgumentValue TraversalPop |
  TraversalPopArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalPopArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalPopArgument")

_TraversalPopArgument_value = (Core.FieldName "value")

_TraversalPopArgument_variable = (Core.FieldName "variable")

data TraversalSackMethodArgument = 
  TraversalSackMethodArgumentValue  |
  TraversalSackMethodArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalSackMethodArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalSackMethodArgument")

_TraversalSackMethodArgument_value = (Core.FieldName "value")

_TraversalSackMethodArgument_variable = (Core.FieldName "variable")

data TraversalScopeArgument = 
  TraversalScopeArgumentValue TraversalScope |
  TraversalScopeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalScopeArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalScopeArgument")

_TraversalScopeArgument_value = (Core.FieldName "value")

_TraversalScopeArgument_variable = (Core.FieldName "variable")

data TraversalTokenArgument = 
  TraversalTokenArgumentValue TraversalToken |
  TraversalTokenArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalTokenArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalTokenArgument")

_TraversalTokenArgument_value = (Core.FieldName "value")

_TraversalTokenArgument_variable = (Core.FieldName "variable")

data TraversalComparatorArgument = 
  TraversalComparatorArgumentValue TraversalOrder |
  TraversalComparatorArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalComparatorArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalComparatorArgument")

_TraversalComparatorArgument_value = (Core.FieldName "value")

_TraversalComparatorArgument_variable = (Core.FieldName "variable")

data TraversalFunctionArgument = 
  TraversalFunctionArgumentValue TraversalFunction |
  TraversalFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalFunctionArgument")

_TraversalFunctionArgument_value = (Core.FieldName "value")

_TraversalFunctionArgument_variable = (Core.FieldName "variable")

data TraversalBiFunctionArgument = 
  TraversalBiFunctionArgumentValue TraversalOperator |
  TraversalBiFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalBiFunctionArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalBiFunctionArgument")

_TraversalBiFunctionArgument_value = (Core.FieldName "value")

_TraversalBiFunctionArgument_variable = (Core.FieldName "variable")

data TraversalDTArgument = 
  TraversalDTArgumentValue TraversalDT |
  TraversalDTArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalDTArgument = (Core.Name "hydra/langs/tinkerpop/gremlin.TraversalDTArgument")

_TraversalDTArgument_value = (Core.FieldName "value")

_TraversalDTArgument_variable = (Core.FieldName "variable")

newtype GenericLiteralList = 
  GenericLiteralList {
    unGenericLiteralList :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralList = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralList")

data GenericLiteralRange = 
  GenericLiteralRangeInteger IntegerRange |
  GenericLiteralRangeString StringRange
  deriving (Eq, Ord, Read, Show)

_GenericLiteralRange = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralRange")

_GenericLiteralRange_integer = (Core.FieldName "integer")

_GenericLiteralRange_string = (Core.FieldName "string")

data IntegerRange = 
  IntegerRange {
    integerRangeLeft :: IntegerLiteral,
    integerRangeRight :: IntegerLiteral}
  deriving (Eq, Ord, Read, Show)

_IntegerRange = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerRange")

_IntegerRange_left = (Core.FieldName "left")

_IntegerRange_right = (Core.FieldName "right")

data StringRange = 
  StringRange {
    stringRangeLeft :: String,
    stringRangeRight :: String}
  deriving (Eq, Ord, Read, Show)

_StringRange = (Core.Name "hydra/langs/tinkerpop/gremlin.StringRange")

_StringRange_left = (Core.FieldName "left")

_StringRange_right = (Core.FieldName "right")

newtype GenericLiteralSet = 
  GenericLiteralSet {
    unGenericLiteralSet :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralSet = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralSet")

newtype GenericLiteralCollection = 
  GenericLiteralCollection {
    unGenericLiteralCollection :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralCollection = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralCollection")

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

_GenericLiteral_numeric = (Core.FieldName "numeric")

_GenericLiteral_boolean = (Core.FieldName "boolean")

_GenericLiteral_string = (Core.FieldName "string")

_GenericLiteral_date = (Core.FieldName "date")

_GenericLiteral_null = (Core.FieldName "null")

_GenericLiteral_nan = (Core.FieldName "nan")

_GenericLiteral_inf = (Core.FieldName "inf")

_GenericLiteral_traversalToken = (Core.FieldName "traversalToken")

_GenericLiteral_traversalCardinality = (Core.FieldName "traversalCardinality")

_GenericLiteral_traversalDirection = (Core.FieldName "traversalDirection")

_GenericLiteral_traversalMerge = (Core.FieldName "traversalMerge")

_GenericLiteral_traversalPick = (Core.FieldName "traversalPick")

_GenericLiteral_traversalDT = (Core.FieldName "traversalDT")

_GenericLiteral_structureVertex = (Core.FieldName "structureVertex")

_GenericLiteral_genericLiteralSet = (Core.FieldName "genericLiteralSet")

_GenericLiteral_genericLiteralCollection = (Core.FieldName "genericLiteralCollection")

_GenericLiteral_genericLiteralRange = (Core.FieldName "genericLiteralRange")

_GenericLiteral_nestedTraversal = (Core.FieldName "nestedTraversal")

_GenericLiteral_terminatedTraversal = (Core.FieldName "terminatedTraversal")

_GenericLiteral_genericLiteralMap = (Core.FieldName "genericLiteralMap")

newtype GenericLiteralMap = 
  GenericLiteralMap {
    unGenericLiteralMap :: [MapEntry]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMap = (Core.Name "hydra/langs/tinkerpop/gremlin.GenericLiteralMap")

data MapEntry = 
  MapEntryKey MapKey |
  MapEntryValue GenericLiteral
  deriving (Eq, Ord, Read, Show)

_MapEntry = (Core.Name "hydra/langs/tinkerpop/gremlin.MapEntry")

_MapEntry_key = (Core.FieldName "key")

_MapEntry_value = (Core.FieldName "value")

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

_MapKey_string = (Core.FieldName "string")

_MapKey_numeric = (Core.FieldName "numeric")

_MapKey_traversalToken = (Core.FieldName "traversalToken")

_MapKey_traversalDirection = (Core.FieldName "traversalDirection")

_MapKey_set = (Core.FieldName "set")

_MapKey_collection = (Core.FieldName "collection")

_MapKey_map = (Core.FieldName "map")

_MapKey_keyword = (Core.FieldName "keyword")

_MapKey_identifier = (Core.FieldName "identifier")

newtype IntegerLiteral = 
  IntegerLiteral {
    unIntegerLiteral :: Integer}
  deriving (Eq, Ord, Read, Show)

_IntegerLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.IntegerLiteral")

newtype FloatLiteral = 
  FloatLiteral {
    unFloatLiteral :: Double}
  deriving (Eq, Ord, Read, Show)

_FloatLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.FloatLiteral")

data NumericLiteral = 
  NumericLiteralInteger IntegerLiteral |
  NumericLiteralFloat FloatLiteral
  deriving (Eq, Ord, Read, Show)

_NumericLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.NumericLiteral")

_NumericLiteral_integer = (Core.FieldName "integer")

_NumericLiteral_float = (Core.FieldName "float")

newtype DateLiteral = 
  DateLiteral {
    unDateLiteral :: (Maybe StringArgument)}
  deriving (Eq, Ord, Read, Show)

_DateLiteral = (Core.Name "hydra/langs/tinkerpop/gremlin.DateLiteral")

data Keyword = 
  KeywordEdges  |
  KeywordKeys  |
  KeywordNew  |
  KeywordValues 
  deriving (Eq, Ord, Read, Show)

_Keyword = (Core.Name "hydra/langs/tinkerpop/gremlin.Keyword")

_Keyword_edges = (Core.FieldName "edges")

_Keyword_keys = (Core.FieldName "keys")

_Keyword_new = (Core.FieldName "new")

_Keyword_values = (Core.FieldName "values")

newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra/langs/tinkerpop/gremlin.Identifier")