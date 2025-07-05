-- | A Gremlin model, based on the Gremlin ANTLR grammar (master branch, as of 2024-06-30).

module Hydra.Ext.Org.Apache.Tinkerpop.Gremlin where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

newtype QueryList = 
  QueryList {
    unQueryList :: [Query]}
  deriving (Eq, Ord, Read, Show)

_QueryList = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.QueryList")

data Query = 
  QueryTraversalSource TraversalSourceQuery |
  QueryRootTraversal RootTraversalQuery |
  QueryToString  |
  QueryEmpty 
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.Query")

_Query_traversalSource = (Core.Name "traversalSource")

_Query_rootTraversal = (Core.Name "rootTraversal")

_Query_toString = (Core.Name "toString")

_Query_empty = (Core.Name "empty")

data TraversalSourceQuery = 
  TraversalSourceQuery {
    traversalSourceQuerySource :: TraversalSource,
    traversalSourceQueryTransactionPart :: (Maybe TransactionPart)}
  deriving (Eq, Ord, Read, Show)

_TraversalSourceQuery = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceQuery")

_TraversalSourceQuery_source = (Core.Name "source")

_TraversalSourceQuery_transactionPart = (Core.Name "transactionPart")

data RootTraversalQuery = 
  RootTraversalQuery {
    rootTraversalQueryRoot :: RootTraversal,
    rootTraversalQueryTerminalMethod :: (Maybe TraversalTerminalMethod)}
  deriving (Eq, Ord, Read, Show)

_RootTraversalQuery = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.RootTraversalQuery")

_RootTraversalQuery_root = (Core.Name "root")

_RootTraversalQuery_terminalMethod = (Core.Name "terminalMethod")

newtype TraversalSource = 
  TraversalSource {
    unTraversalSource :: [TraversalSourceSelfMethod]}
  deriving (Eq, Ord, Read, Show)

_TraversalSource = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource")

data TransactionPart = 
  TransactionPartBegin  |
  TransactionPartCommit  |
  TransactionPartRollback 
  deriving (Eq, Ord, Read, Show)

_TransactionPart = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TransactionPart")

_TransactionPart_begin = (Core.Name "begin")

_TransactionPart_commit = (Core.Name "commit")

_TransactionPart_rollback = (Core.Name "rollback")

data RootTraversal = 
  RootTraversal {
    rootTraversalSource :: TraversalSource,
    rootTraversalSpawnMethod :: TraversalSourceSpawnMethod,
    rootTraversalChained :: [ChainedTraversalElement]}
  deriving (Eq, Ord, Read, Show)

_RootTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal")

_RootTraversal_source = (Core.Name "source")

_RootTraversal_spawnMethod = (Core.Name "spawnMethod")

_RootTraversal_chained = (Core.Name "chained")

data TraversalSourceSelfMethod = 
  TraversalSourceSelfMethodWithBulk Bool |
  TraversalSourceSelfMethodWithPath  |
  TraversalSourceSelfMethodWithSack GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument |
  TraversalSourceSelfMethodWithSideEffect StringArgumentAndGenericLiteralArgument |
  TraversalSourceSelfMethodWithStrategies [TraversalStrategy] |
  TraversalSourceSelfMethodWithoutStrategies [Identifier] |
  TraversalSourceSelfMethodWith StringArgumentAndOptionalGenericLiteralArgument
  deriving (Eq, Ord, Read, Show)

_TraversalSourceSelfMethod = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod")

_TraversalSourceSelfMethod_withBulk = (Core.Name "withBulk")

_TraversalSourceSelfMethod_withPath = (Core.Name "withPath")

_TraversalSourceSelfMethod_withSack = (Core.Name "withSack")

_TraversalSourceSelfMethod_withSideEffect = (Core.Name "withSideEffect")

_TraversalSourceSelfMethod_withStrategies = (Core.Name "withStrategies")

_TraversalSourceSelfMethod_withoutStrategies = (Core.Name "withoutStrategies")

_TraversalSourceSelfMethod_with = (Core.Name "with")

data GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument = 
  GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument {
    genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentLiteral :: GenericLiteralArgument,
    genericLiteralArgumentAndOptionalTraversalBiFunctionArgumentBiFunction :: (Maybe TraversalBiFunctionArgument)}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument")

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_literal = (Core.Name "literal")

_GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument_biFunction = (Core.Name "biFunction")

data StringArgumentAndGenericLiteralArgument = 
  StringArgumentAndGenericLiteralArgument {
    stringArgumentAndGenericLiteralArgumentString :: StringArgument,
    stringArgumentAndGenericLiteralArgumentLiteral :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndGenericLiteralArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument")

_StringArgumentAndGenericLiteralArgument_string = (Core.Name "string")

_StringArgumentAndGenericLiteralArgument_literal = (Core.Name "literal")

data StringArgumentAndOptionalGenericLiteralArgument = 
  StringArgumentAndOptionalGenericLiteralArgument {
    stringArgumentAndOptionalGenericLiteralArgumentString :: StringArgument,
    stringArgumentAndOptionalGenericLiteralArgumentLiteral :: (Maybe GenericLiteralArgument)}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndOptionalGenericLiteralArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument")

_StringArgumentAndOptionalGenericLiteralArgument_string = (Core.Name "string")

_StringArgumentAndOptionalGenericLiteralArgument_literal = (Core.Name "literal")

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

_TraversalSourceSpawnMethod = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod")

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

data GenericLiteralMapNullableArgumentOrNestedTraversal = 
  GenericLiteralMapNullableArgumentOrNestedTraversalMap GenericLiteralMapNullableArgument |
  GenericLiteralMapNullableArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgumentOrNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal")

_GenericLiteralMapNullableArgumentOrNestedTraversal_map = (Core.Name "map")

_GenericLiteralMapNullableArgumentOrNestedTraversal_traversal = (Core.Name "traversal")

data ServiceCall = 
  ServiceCall {
    serviceCallService :: StringArgument,
    serviceCallArguments :: ServiceArguments}
  deriving (Eq, Ord, Read, Show)

_ServiceCall = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ServiceCall")

_ServiceCall_service = (Core.Name "service")

_ServiceCall_arguments = (Core.Name "arguments")

data ServiceArguments = 
  ServiceArgumentsMap (Maybe GenericLiteralMapArgument) |
  ServiceArgumentsTraversal (Maybe NestedTraversal)
  deriving (Eq, Ord, Read, Show)

_ServiceArguments = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ServiceArguments")

_ServiceArguments_map = (Core.Name "map")

_ServiceArguments_traversal = (Core.Name "traversal")

data ChainedTraversal = 
  ChainedTraversal {
    chainedTraversalFirst :: TraversalMethod,
    chainedTraversalRest :: ChainedTraversalElement}
  deriving (Eq, Ord, Read, Show)

_ChainedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversal")

_ChainedTraversal_first = (Core.Name "first")

_ChainedTraversal_rest = (Core.Name "rest")

data ChainedTraversalElement = 
  ChainedTraversalElementMethod TraversalMethod |
  ChainedTraversalElementSelf TraversalSelfMethod
  deriving (Eq, Ord, Read, Show)

_ChainedTraversalElement = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement")

_ChainedTraversalElement_method = (Core.Name "method")

_ChainedTraversalElement_self = (Core.Name "self")

data NestedTraversal = 
  NestedTraversalRoot RootTraversal |
  NestedTraversalChained ChainedTraversal |
  NestedTraversalAnonymous ChainedTraversal
  deriving (Eq, Ord, Read, Show)

_NestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal")

_NestedTraversal_root = (Core.Name "root")

_NestedTraversal_chained = (Core.Name "chained")

_NestedTraversal_anonymous = (Core.Name "anonymous")

data TerminatedTraversal = 
  TerminatedTraversal {
    terminatedTraversalRoot :: RootTraversal,
    terminatedTraversalTerminal :: TraversalTerminalMethod}
  deriving (Eq, Ord, Read, Show)

_TerminatedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TerminatedTraversal")

_TerminatedTraversal_root = (Core.Name "root")

_TerminatedTraversal_terminal = (Core.Name "terminal")

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

_TraversalMethod = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod")

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

data StringArgumentOrNestedTraversal = 
  StringArgumentOrNestedTraversalString StringArgument |
  StringArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_StringArgumentOrNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal")

_StringArgumentOrNestedTraversal_string = (Core.Name "string")

_StringArgumentOrNestedTraversal_traversal = (Core.Name "traversal")

data OptionalTraversalScopeArgumentAndStringArgument = 
  OptionalTraversalScopeArgumentAndStringArgument {
    optionalTraversalScopeArgumentAndStringArgumentScope :: (Maybe TraversalScopeArgument),
    optionalTraversalScopeArgumentAndStringArgumentString :: StringArgument}
  deriving (Eq, Ord, Read, Show)

_OptionalTraversalScopeArgumentAndStringArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument")

_OptionalTraversalScopeArgumentAndStringArgument_scope = (Core.Name "scope")

_OptionalTraversalScopeArgumentAndStringArgument_string = (Core.Name "string")

data StringArgumentAndOptionalStringLiteralVarargs = 
  StringArgumentAndOptionalStringLiteralVarargs {
    stringArgumentAndOptionalStringLiteralVarargsFirst :: StringArgument,
    stringArgumentAndOptionalStringLiteralVarargsRest :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_StringArgumentAndOptionalStringLiteralVarargs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs")

_StringArgumentAndOptionalStringLiteralVarargs_first = (Core.Name "first")

_StringArgumentAndOptionalStringLiteralVarargs_rest = (Core.Name "rest")

data TraversalSackMethodArgumentOrIntegerArgument = 
  TraversalSackMethodArgumentOrIntegerArgumentConsumer TraversalSackMethodArgument |
  TraversalSackMethodArgumentOrIntegerArgumentInt IntegerArgument
  deriving (Eq, Ord, Read, Show)

_TraversalSackMethodArgumentOrIntegerArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument")

_TraversalSackMethodArgumentOrIntegerArgument_consumer = (Core.Name "consumer")

_TraversalSackMethodArgumentOrIntegerArgument_int = (Core.Name "int")

data ByArgs = 
  ByArgsOrder TraversalOrderArgument |
  ByArgsToken TraversalTokenArgument |
  ByArgsOther ByOtherArgs
  deriving (Eq, Ord, Read, Show)

_ByArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ByArgs")

_ByArgs_order = (Core.Name "order")

_ByArgs_token = (Core.Name "token")

_ByArgs_other = (Core.Name "other")

data ByOtherArgs = 
  ByOtherArgsComparator (Maybe TraversalComparatorArgument) |
  ByOtherArgsOther (Maybe TraversalFunctionArgumentOrStringArgumentOrNestedTraversal)
  deriving (Eq, Ord, Read, Show)

_ByOtherArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ByOtherArgs")

_ByOtherArgs_comparator = (Core.Name "comparator")

_ByOtherArgs_other = (Core.Name "other")

data TraversalFunctionArgumentOrStringArgumentOrNestedTraversal = 
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction TraversalFunctionArgument |
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalString StringArgument |
  TraversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_function = (Core.Name "function")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_string = (Core.Name "string")

_TraversalFunctionArgumentOrStringArgumentOrNestedTraversal_traversal = (Core.Name "traversal")

data ChooseArgs = 
  ChooseArgsFunction TraversalFunctionArgument |
  ChooseArgsPredicateTraversal PredicateTraversalArgument |
  ChooseArgsTraversal NestedTraversalArgument
  deriving (Eq, Ord, Read, Show)

_ChooseArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ChooseArgs")

_ChooseArgs_function = (Core.Name "function")

_ChooseArgs_predicateTraversal = (Core.Name "predicateTraversal")

_ChooseArgs_traversal = (Core.Name "traversal")

data PredicateTraversalArgument = 
  PredicateTraversalArgument {
    predicateTraversalArgumentPredicate :: TraversalPredicate,
    predicateTraversalArgumentTraversal1 :: NestedTraversal,
    predicateTraversalArgumentTraversal2 :: (Maybe NestedTraversal)}
  deriving (Eq, Ord, Read, Show)

_PredicateTraversalArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.PredicateTraversalArgument")

_PredicateTraversalArgument_predicate = (Core.Name "predicate")

_PredicateTraversalArgument_traversal1 = (Core.Name "traversal1")

_PredicateTraversalArgument_traversal2 = (Core.Name "traversal2")

data NestedTraversalArgument = 
  NestedTraversalArgument {
    nestedTraversalArgumentTraversal1 :: NestedTraversal,
    nestedTraversalArgumentTraversal2 :: (Maybe NestedTraversal),
    nestedTraversalArgumentTraversal3 :: (Maybe NestedTraversal)}
  deriving (Eq, Ord, Read, Show)

_NestedTraversalArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversalArgument")

_NestedTraversalArgument_traversal1 = (Core.Name "traversal1")

_NestedTraversalArgument_traversal2 = (Core.Name "traversal2")

_NestedTraversalArgument_traversal3 = (Core.Name "traversal3")

data DedupArgs = 
  DedupArgsScopeString ScopeStringArgument |
  DedupArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_DedupArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.DedupArgs")

_DedupArgs_scopeString = (Core.Name "scopeString")

_DedupArgs_string = (Core.Name "string")

data ScopeStringArgument = 
  ScopeStringArgument {
    scopeStringArgumentScope :: TraversalScopeArgument,
    scopeStringArgumentStrings :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_ScopeStringArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ScopeStringArgument")

_ScopeStringArgument_scope = (Core.Name "scope")

_ScopeStringArgument_strings = (Core.Name "strings")

data PredicateOrTraversal = 
  PredicateOrTraversalPredicate TraversalPredicate |
  PredicateOrTraversalTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_PredicateOrTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal")

_PredicateOrTraversal_predicate = (Core.Name "predicate")

_PredicateOrTraversal_traversal = (Core.Name "traversal")

data GenericLiteralArgumentAndTraversalBiFunctionArgument = 
  GenericLiteralArgumentAndTraversalBiFunctionArgument {
    genericLiteralArgumentAndTraversalBiFunctionArgumentLiteral :: GenericLiteralArgument,
    genericLiteralArgumentAndTraversalBiFunctionArgumentBiFunction :: TraversalBiFunctionArgument}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndTraversalBiFunctionArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument")

_GenericLiteralArgumentAndTraversalBiFunctionArgument_literal = (Core.Name "literal")

_GenericLiteralArgumentAndTraversalBiFunctionArgument_biFunction = (Core.Name "biFunction")

data FromArgs = 
  FromArgsString StringArgument |
  FromArgsVertex StructureVertexArgument |
  FromArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_FromArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.FromArgs")

_FromArgs_string = (Core.Name "string")

_FromArgs_vertex = (Core.Name "vertex")

_FromArgs_traversal = (Core.Name "traversal")

data HasArgs = 
  HasArgsString HasStringArgumentAndOptionalStringLiteralVarargs |
  HasArgsTraversalToken HasTraversalTokenArgs
  deriving (Eq, Ord, Read, Show)

_HasArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.HasArgs")

_HasArgs_string = (Core.Name "string")

_HasArgs_traversalToken = (Core.Name "traversalToken")

data HasStringArgumentAndOptionalStringLiteralVarargs = 
  HasStringArgumentAndOptionalStringLiteralVarargs {
    hasStringArgumentAndOptionalStringLiteralVarargsString :: StringNullableArgument,
    hasStringArgumentAndOptionalStringLiteralVarargsRest :: (Maybe HasStringArgumentAndOptionalStringLiteralVarargsRest)}
  deriving (Eq, Ord, Read, Show)

_HasStringArgumentAndOptionalStringLiteralVarargs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs")

_HasStringArgumentAndOptionalStringLiteralVarargs_string = (Core.Name "string")

_HasStringArgumentAndOptionalStringLiteralVarargs_rest = (Core.Name "rest")

data HasStringArgumentAndOptionalStringLiteralVarargsRest = 
  HasStringArgumentAndOptionalStringLiteralVarargsRestObject GenericLiteralArgument |
  HasStringArgumentAndOptionalStringLiteralVarargsRestPredicate TraversalPredicate |
  HasStringArgumentAndOptionalStringLiteralVarargsRestStringObject StringNullableArgumentAndGenericLiteralArgument |
  HasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate StringNullableArgumentAndTraversalPredicate |
  HasStringArgumentAndOptionalStringLiteralVarargsRestTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_HasStringArgumentAndOptionalStringLiteralVarargsRest = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_object = (Core.Name "object")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_predicate = (Core.Name "predicate")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_stringObject = (Core.Name "stringObject")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_stringPredicate = (Core.Name "stringPredicate")

_HasStringArgumentAndOptionalStringLiteralVarargsRest_traversal = (Core.Name "traversal")

data StringNullableArgumentAndGenericLiteralArgument = 
  StringNullableArgumentAndGenericLiteralArgument {
    stringNullableArgumentAndGenericLiteralArgumentString :: StringNullableArgument,
    stringNullableArgumentAndGenericLiteralArgumentLiteral :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_StringNullableArgumentAndGenericLiteralArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument")

_StringNullableArgumentAndGenericLiteralArgument_string = (Core.Name "string")

_StringNullableArgumentAndGenericLiteralArgument_literal = (Core.Name "literal")

data StringNullableArgumentAndTraversalPredicate = 
  StringNullableArgumentAndTraversalPredicate {
    stringNullableArgumentAndTraversalPredicateString :: StringNullableArgument,
    stringNullableArgumentAndTraversalPredicatePredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_StringNullableArgumentAndTraversalPredicate = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate")

_StringNullableArgumentAndTraversalPredicate_string = (Core.Name "string")

_StringNullableArgumentAndTraversalPredicate_predicate = (Core.Name "predicate")

data HasTraversalTokenArgs = 
  HasTraversalTokenArgs {
    hasTraversalTokenArgsTraversalToken :: TraversalTokenArgument,
    hasTraversalTokenArgsRest :: HasTraversalTokenArgsRest}
  deriving (Eq, Ord, Read, Show)

_HasTraversalTokenArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgs")

_HasTraversalTokenArgs_traversalToken = (Core.Name "traversalToken")

_HasTraversalTokenArgs_rest = (Core.Name "rest")

data HasTraversalTokenArgsRest = 
  HasTraversalTokenArgsRestLiteral GenericLiteralArgument |
  HasTraversalTokenArgsRestPredicate TraversalPredicate |
  HasTraversalTokenArgsRestTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_HasTraversalTokenArgsRest = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest")

_HasTraversalTokenArgsRest_literal = (Core.Name "literal")

_HasTraversalTokenArgsRest_predicate = (Core.Name "predicate")

_HasTraversalTokenArgsRest_traversal = (Core.Name "traversal")

data GenericLiteralArgumentAndTraversalPredicate = 
  GenericLiteralArgumentAndTraversalPredicateLiteral GenericLiteralArgument |
  GenericLiteralArgumentAndTraversalPredicatePredicate TraversalPredicate
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndTraversalPredicate = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate")

_GenericLiteralArgumentAndTraversalPredicate_literal = (Core.Name "literal")

_GenericLiteralArgumentAndTraversalPredicate_predicate = (Core.Name "predicate")

data TraversalPredicateOrStringLiteralVarargs = 
  TraversalPredicateOrStringLiteralVarargsPredicate TraversalPredicate |
  TraversalPredicateOrStringLiteralVarargsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateOrStringLiteralVarargs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs")

_TraversalPredicateOrStringLiteralVarargs_predicate = (Core.Name "predicate")

_TraversalPredicateOrStringLiteralVarargs_string = (Core.Name "string")

data TraversalPredicateOrGenericLiteralArgument = 
  TraversalPredicateOrGenericLiteralArgumentPredicate TraversalPredicate |
  TraversalPredicateOrGenericLiteralArgumentLiteral [GenericLiteralArgument]
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateOrGenericLiteralArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument")

_TraversalPredicateOrGenericLiteralArgument_predicate = (Core.Name "predicate")

_TraversalPredicateOrGenericLiteralArgument_literal = (Core.Name "literal")

data OptionArgs = 
  OptionArgsPredicateTraversal TraversalPredicateAndNestedTraversal |
  OptionArgsMergeMap TraversalMergeArgumentAndGenericLiteralMapNullableArgument |
  OptionArgsMergeTraversal TraversalMergeArgumentAndNestedTraversal |
  OptionArgsObjectTraversal GenericLiteralArgumentAndNestedTraversal |
  OptionArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_OptionArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs")

_OptionArgs_predicateTraversal = (Core.Name "predicateTraversal")

_OptionArgs_mergeMap = (Core.Name "mergeMap")

_OptionArgs_mergeTraversal = (Core.Name "mergeTraversal")

_OptionArgs_objectTraversal = (Core.Name "objectTraversal")

_OptionArgs_traversal = (Core.Name "traversal")

data TraversalPredicateAndNestedTraversal = 
  TraversalPredicateAndNestedTraversal {
    traversalPredicateAndNestedTraversalPredicate :: TraversalPredicate,
    traversalPredicateAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalPredicateAndNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal")

_TraversalPredicateAndNestedTraversal_predicate = (Core.Name "predicate")

_TraversalPredicateAndNestedTraversal_traversal = (Core.Name "traversal")

data TraversalMergeArgumentAndGenericLiteralMapNullableArgument = 
  TraversalMergeArgumentAndGenericLiteralMapNullableArgument {
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentMerge :: TraversalMergeArgument,
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentMap :: GenericLiteralMapNullableArgument,
    traversalMergeArgumentAndGenericLiteralMapNullableArgumentCardinality :: (Maybe TraversalCardinality)}
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_merge = (Core.Name "merge")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_map = (Core.Name "map")

_TraversalMergeArgumentAndGenericLiteralMapNullableArgument_cardinality = (Core.Name "cardinality")

data TraversalMergeArgumentAndNestedTraversal = 
  TraversalMergeArgumentAndNestedTraversal {
    traversalMergeArgumentAndNestedTraversalMerge :: TraversalMergeArgument,
    traversalMergeArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgumentAndNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal")

_TraversalMergeArgumentAndNestedTraversal_merge = (Core.Name "merge")

_TraversalMergeArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

data GenericLiteralArgumentAndNestedTraversal = 
  GenericLiteralArgumentAndNestedTraversal {
    genericLiteralArgumentAndNestedTraversalObject :: GenericLiteralArgument,
    genericLiteralArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgumentAndNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal")

_GenericLiteralArgumentAndNestedTraversal_object = (Core.Name "object")

_GenericLiteralArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

data PropertyArgs = 
  PropertyArgsCardinalityObjects TraversalCardinalityArgumentAndObjects |
  PropertyArgsObjects [GenericLiteralArgument] |
  PropertyArgsObject GenericLiteralMapNullableArgument |
  PropertyArgsCardinalityObject GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument
  deriving (Eq, Ord, Read, Show)

_PropertyArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs")

_PropertyArgs_cardinalityObjects = (Core.Name "cardinalityObjects")

_PropertyArgs_objects = (Core.Name "objects")

_PropertyArgs_object = (Core.Name "object")

_PropertyArgs_cardinalityObject = (Core.Name "cardinalityObject")

data TraversalCardinalityArgumentAndObjects = 
  TraversalCardinalityArgumentAndObjects {
    traversalCardinalityArgumentAndObjectsCardinality :: TraversalCardinalityArgument,
    traversalCardinalityArgumentAndObjectsObjects :: [GenericLiteralArgument]}
  deriving (Eq, Ord, Read, Show)

_TraversalCardinalityArgumentAndObjects = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects")

_TraversalCardinalityArgumentAndObjects_cardinality = (Core.Name "cardinality")

_TraversalCardinalityArgumentAndObjects_objects = (Core.Name "objects")

data GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument = 
  GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument {
    genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentCardinality :: TraversalCardinalityArgument,
    genericLiteralMapNullableArgumentAndTraversalCardinalityArgumentObject :: GenericLiteralMapNullableArgument}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument")

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_cardinality = (Core.Name "cardinality")

_GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument_object = (Core.Name "object")

data RangeArgs = 
  RangeArgs {
    rangeArgsScope :: (Maybe TraversalScopeArgument),
    rangeArgsMin :: IntegerArgument,
    rangeArgsMax :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_RangeArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.RangeArgs")

_RangeArgs_scope = (Core.Name "scope")

_RangeArgs_min = (Core.Name "min")

_RangeArgs_max = (Core.Name "max")

data OptionalStringArgumentAndNestedTraversal = 
  OptionalStringArgumentAndNestedTraversal {
    optionalStringArgumentAndNestedTraversalString :: (Maybe StringArgument),
    optionalStringArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_OptionalStringArgumentAndNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal")

_OptionalStringArgumentAndNestedTraversal_string = (Core.Name "string")

_OptionalStringArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

data SelectArgs = 
  SelectArgsColumn TraversalColumnArgument |
  SelectArgsPopStrings PopStringsArgument |
  SelectArgsPopTraversal TraversalPopArgumentAndNestedTraversal |
  SelectArgsStrings [StringArgument] |
  SelectArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_SelectArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.SelectArgs")

_SelectArgs_column = (Core.Name "column")

_SelectArgs_popStrings = (Core.Name "popStrings")

_SelectArgs_popTraversal = (Core.Name "popTraversal")

_SelectArgs_strings = (Core.Name "strings")

_SelectArgs_traversal = (Core.Name "traversal")

data PopStringsArgument = 
  PopStringsArgument {
    popStringsArgumentPop :: TraversalPopArgument,
    popStringsArgumentString :: [StringArgument]}
  deriving (Eq, Ord, Read, Show)

_PopStringsArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.PopStringsArgument")

_PopStringsArgument_pop = (Core.Name "pop")

_PopStringsArgument_string = (Core.Name "string")

data TraversalPopArgumentAndNestedTraversal = 
  TraversalPopArgumentAndNestedTraversal {
    traversalPopArgumentAndNestedTraversalPop :: TraversalPopArgument,
    traversalPopArgumentAndNestedTraversalTraversal :: NestedTraversal}
  deriving (Eq, Ord, Read, Show)

_TraversalPopArgumentAndNestedTraversal = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal")

_TraversalPopArgumentAndNestedTraversal_pop = (Core.Name "pop")

_TraversalPopArgumentAndNestedTraversal_traversal = (Core.Name "traversal")

data OptionalTraversalScopeArgumentAndIntegerArgument = 
  OptionalTraversalScopeArgumentAndIntegerArgument {
    optionalTraversalScopeArgumentAndIntegerArgumentScope :: (Maybe TraversalScopeArgument),
    optionalTraversalScopeArgumentAndIntegerArgumentLong :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_OptionalTraversalScopeArgumentAndIntegerArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument")

_OptionalTraversalScopeArgumentAndIntegerArgument_scope = (Core.Name "scope")

_OptionalTraversalScopeArgumentAndIntegerArgument_long = (Core.Name "long")

data TailArgs = 
  TailArgs {
    tailArgsScope :: (Maybe TraversalScopeArgument),
    tailArgsInteger :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)

_TailArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TailArgs")

_TailArgs_scope = (Core.Name "scope")

_TailArgs_integer = (Core.Name "integer")

data ToArgs = 
  ToArgsDirection DirectionAndVarargs |
  ToArgsString StringArgument |
  ToArgsVertex StructureVertexArgument |
  ToArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_ToArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ToArgs")

_ToArgs_direction = (Core.Name "direction")

_ToArgs_string = (Core.Name "string")

_ToArgs_vertex = (Core.Name "vertex")

_ToArgs_traversal = (Core.Name "traversal")

data DirectionAndVarargs = 
  DirectionAndVarargs {
    directionAndVarargsDirection :: TraversalDirectionArgument,
    directionAndVarargsVarargs :: [StringNullableArgument]}
  deriving (Eq, Ord, Read, Show)

_DirectionAndVarargs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.DirectionAndVarargs")

_DirectionAndVarargs_direction = (Core.Name "direction")

_DirectionAndVarargs_varargs = (Core.Name "varargs")

data ValueMapArgs = 
  ValueMapArgsString [StringNullableArgument] |
  ValueMapArgsBoolean ValueMapBooleanArgs
  deriving (Eq, Ord, Read, Show)

_ValueMapArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ValueMapArgs")

_ValueMapArgs_string = (Core.Name "string")

_ValueMapArgs_boolean = (Core.Name "boolean")

data ValueMapBooleanArgs = 
  ValueMapBooleanArgs {
    valueMapBooleanArgsValue :: BooleanArgument,
    valueMapBooleanArgsKeys :: (Maybe [StringNullableArgument])}
  deriving (Eq, Ord, Read, Show)

_ValueMapBooleanArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ValueMapBooleanArgs")

_ValueMapBooleanArgs_value = (Core.Name "value")

_ValueMapBooleanArgs_keys = (Core.Name "keys")

data WhereArgs = 
  WhereArgsPredicate WhereWithPredicateArgs |
  WhereArgsString StringArgument |
  WhereArgsTraversal NestedTraversal
  deriving (Eq, Ord, Read, Show)

_WhereArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.WhereArgs")

_WhereArgs_predicate = (Core.Name "predicate")

_WhereArgs_string = (Core.Name "string")

_WhereArgs_traversal = (Core.Name "traversal")

data WhereWithPredicateArgs = 
  WhereWithPredicateArgs {
    whereWithPredicateArgsLeftArg :: (Maybe StringArgument),
    whereWithPredicateArgsPredicate :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_WhereWithPredicateArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.WhereWithPredicateArgs")

_WhereWithPredicateArgs_leftArg = (Core.Name "leftArg")

_WhereWithPredicateArgs_predicate = (Core.Name "predicate")

data WithArgs = 
  WithArgs {
    withArgsKeys :: WithArgsKeys,
    withArgsValues :: (Maybe WithArgsValues)}
  deriving (Eq, Ord, Read, Show)

_WithArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.WithArgs")

_WithArgs_keys = (Core.Name "keys")

_WithArgs_values = (Core.Name "values")

data WithArgsKeys = 
  WithArgsKeysWithOption WithOptionKeys |
  WithArgsKeysString StringArgument
  deriving (Eq, Ord, Read, Show)

_WithArgsKeys = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys")

_WithArgsKeys_withOption = (Core.Name "withOption")

_WithArgsKeys_string = (Core.Name "string")

data WithArgsValues = 
  WithArgsValuesWithOptions WithOptionsValues |
  WithArgsValuesIo IoOptionsValues |
  WithArgsValuesObject GenericLiteralArgument
  deriving (Eq, Ord, Read, Show)

_WithArgsValues = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues")

_WithArgsValues_withOptions = (Core.Name "withOptions")

_WithArgsValues_io = (Core.Name "io")

_WithArgsValues_object = (Core.Name "object")

data ConcatArgs = 
  ConcatArgsTraversal [NestedTraversal] |
  ConcatArgsString [StringNullableArgument]
  deriving (Eq, Ord, Read, Show)

_ConcatArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ConcatArgs")

_ConcatArgs_traversal = (Core.Name "traversal")

_ConcatArgs_string = (Core.Name "string")

data ReplaceArgs = 
  ReplaceArgs {
    replaceArgsScope :: (Maybe TraversalScopeArgument),
    replaceArgsFrom :: StringNullableArgument,
    replaceArgsTo :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)

_ReplaceArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ReplaceArgs")

_ReplaceArgs_scope = (Core.Name "scope")

_ReplaceArgs_from = (Core.Name "from")

_ReplaceArgs_to = (Core.Name "to")

data SplitArgs = 
  SplitArgs {
    splitArgsScope :: (Maybe TraversalScopeArgument),
    splitArgsDelimiter :: StringNullableArgument}
  deriving (Eq, Ord, Read, Show)

_SplitArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.SplitArgs")

_SplitArgs_scope = (Core.Name "scope")

_SplitArgs_delimiter = (Core.Name "delimiter")

data SubstringArgs = 
  SubstringArgs {
    substringArgsScope :: (Maybe TraversalScopeArgument),
    substringArgsStart :: IntegerArgument,
    substringArgsEnd :: (Maybe IntegerArgument)}
  deriving (Eq, Ord, Read, Show)

_SubstringArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.SubstringArgs")

_SubstringArgs_scope = (Core.Name "scope")

_SubstringArgs_start = (Core.Name "start")

_SubstringArgs_end = (Core.Name "end")

data DateAddArgs = 
  DateAddArgs {
    dateAddArgsUnit :: TraversalDTArgument,
    dateAddArgsDuration :: IntegerArgument}
  deriving (Eq, Ord, Read, Show)

_DateAddArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.DateAddArgs")

_DateAddArgs_unit = (Core.Name "unit")

_DateAddArgs_duration = (Core.Name "duration")

data DateDiffArgs = 
  DateDiffArgsTraversal NestedTraversal |
  DateDiffArgsDate DateArgument
  deriving (Eq, Ord, Read, Show)

_DateDiffArgs = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.DateDiffArgs")

_DateDiffArgs_traversal = (Core.Name "traversal")

_DateDiffArgs_date = (Core.Name "date")

data StructureVertex = 
  StructureVertex {
    structureVertexNew :: Bool,
    structureVertexId :: GenericLiteralArgument,
    structureVertexLabel :: StringArgument}
  deriving (Eq, Ord, Read, Show)

_StructureVertex = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StructureVertex")

_StructureVertex_new = (Core.Name "new")

_StructureVertex_id = (Core.Name "id")

_StructureVertex_label = (Core.Name "label")

data TraversalStrategy = 
  TraversalStrategy {
    traversalStrategyNew :: Bool,
    traversalStrategyClass :: Identifier,
    traversalStrategyConfigurations :: [Configuration]}
  deriving (Eq, Ord, Read, Show)

_TraversalStrategy = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalStrategy")

_TraversalStrategy_new = (Core.Name "new")

_TraversalStrategy_class = (Core.Name "class")

_TraversalStrategy_configurations = (Core.Name "configurations")

data Configuration = 
  Configuration {
    configurationKey :: KeywordOrIdentifier,
    configurationValue :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_Configuration = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.Configuration")

_Configuration_key = (Core.Name "key")

_Configuration_value = (Core.Name "value")

data KeywordOrIdentifier = 
  KeywordOrIdentifierKeyword Keyword |
  KeywordOrIdentifierIdentifier Identifier
  deriving (Eq, Ord, Read, Show)

_KeywordOrIdentifier = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.KeywordOrIdentifier")

_KeywordOrIdentifier_keyword = (Core.Name "keyword")

_KeywordOrIdentifier_identifier = (Core.Name "identifier")

data TraversalScope = 
  TraversalScopeLocal  |
  TraversalScopeGlobal 
  deriving (Eq, Ord, Read, Show)

_TraversalScope = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalScope")

_TraversalScope_local = (Core.Name "local")

_TraversalScope_global = (Core.Name "global")

data TraversalToken = 
  TraversalTokenId  |
  TraversalTokenLabel  |
  TraversalTokenKey  |
  TraversalTokenValue 
  deriving (Eq, Ord, Read, Show)

_TraversalToken = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken")

_TraversalToken_id = (Core.Name "id")

_TraversalToken_label = (Core.Name "label")

_TraversalToken_key = (Core.Name "key")

_TraversalToken_value = (Core.Name "value")

data TraversalMerge = 
  TraversalMergeOnCreate  |
  TraversalMergeOnMatch  |
  TraversalMergeOutV  |
  TraversalMergeInV 
  deriving (Eq, Ord, Read, Show)

_TraversalMerge = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalMerge")

_TraversalMerge_onCreate = (Core.Name "onCreate")

_TraversalMerge_onMatch = (Core.Name "onMatch")

_TraversalMerge_outV = (Core.Name "outV")

_TraversalMerge_inV = (Core.Name "inV")

data TraversalOrder = 
  TraversalOrderIncr  |
  TraversalOrderDecr  |
  TraversalOrderAsc  |
  TraversalOrderDesc  |
  TraversalOrderShuffle 
  deriving (Eq, Ord, Read, Show)

_TraversalOrder = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalOrder")

_TraversalOrder_incr = (Core.Name "incr")

_TraversalOrder_decr = (Core.Name "decr")

_TraversalOrder_asc = (Core.Name "asc")

_TraversalOrder_desc = (Core.Name "desc")

_TraversalOrder_shuffle = (Core.Name "shuffle")

data TraversalDirection = 
  TraversalDirectionIn  |
  TraversalDirectionOut  |
  TraversalDirectionBoth 
  deriving (Eq, Ord, Read, Show)

_TraversalDirection = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirection")

_TraversalDirection_in = (Core.Name "in")

_TraversalDirection_out = (Core.Name "out")

_TraversalDirection_both = (Core.Name "both")

data TraversalCardinality = 
  TraversalCardinalitySingle GenericLiteral |
  TraversalCardinalitySet GenericLiteral |
  TraversalCardinalityList GenericLiteral
  deriving (Eq, Ord, Read, Show)

_TraversalCardinality = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality")

_TraversalCardinality_single = (Core.Name "single")

_TraversalCardinality_set = (Core.Name "set")

_TraversalCardinality_list = (Core.Name "list")

data TraversalColumn = 
  TraversalColumnKeys  |
  TraversalColumnValues 
  deriving (Eq, Ord, Read, Show)

_TraversalColumn = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalColumn")

_TraversalColumn_keys = (Core.Name "keys")

_TraversalColumn_values = (Core.Name "values")

data TraversalPop = 
  TraversalPopFirst  |
  TraversalPopLast  |
  TraversalPopAll  |
  TraversalPopMixed 
  deriving (Eq, Ord, Read, Show)

_TraversalPop = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPop")

_TraversalPop_first = (Core.Name "first")

_TraversalPop_last = (Core.Name "last")

_TraversalPop_all = (Core.Name "all")

_TraversalPop_mixed = (Core.Name "mixed")

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

_TraversalOperator = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalOperator")

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

data TraversalPick = 
  TraversalPickAny  |
  TraversalPickNone 
  deriving (Eq, Ord, Read, Show)

_TraversalPick = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPick")

_TraversalPick_any = (Core.Name "any")

_TraversalPick_none = (Core.Name "none")

data TraversalDT = 
  TraversalDTSecond  |
  TraversalDTMinute  |
  TraversalDTHour  |
  TraversalDTDay 
  deriving (Eq, Ord, Read, Show)

_TraversalDT = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT")

_TraversalDT_second = (Core.Name "second")

_TraversalDT_minute = (Core.Name "minute")

_TraversalDT_hour = (Core.Name "hour")

_TraversalDT_day = (Core.Name "day")

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

_TraversalPredicate = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate")

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

data TwoTraversalPredicates = 
  TwoTraversalPredicates {
    twoTraversalPredicatesLeft :: TraversalPredicate,
    twoTraversalPredicatesRight :: TraversalPredicate}
  deriving (Eq, Ord, Read, Show)

_TwoTraversalPredicates = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TwoTraversalPredicates")

_TwoTraversalPredicates_left = (Core.Name "left")

_TwoTraversalPredicates_right = (Core.Name "right")

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

_TraversalTerminalMethod = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod")

_TraversalTerminalMethod_explain = (Core.Name "explain")

_TraversalTerminalMethod_iterate = (Core.Name "iterate")

_TraversalTerminalMethod_hasNext = (Core.Name "hasNext")

_TraversalTerminalMethod_tryNext = (Core.Name "tryNext")

_TraversalTerminalMethod_next = (Core.Name "next")

_TraversalTerminalMethod_toList = (Core.Name "toList")

_TraversalTerminalMethod_toSet = (Core.Name "toSet")

_TraversalTerminalMethod_toBulkSet = (Core.Name "toBulkSet")

data TraversalSelfMethod = 
  TraversalSelfMethodDiscard 
  deriving (Eq, Ord, Read, Show)

_TraversalSelfMethod = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalSelfMethod")

_TraversalSelfMethod_discard = (Core.Name "discard")

data TraversalFunction = 
  TraversalFunctionToken TraversalToken |
  TraversalFunctionColumn TraversalColumn
  deriving (Eq, Ord, Read, Show)

_TraversalFunction = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunction")

_TraversalFunction_token = (Core.Name "token")

_TraversalFunction_column = (Core.Name "column")

data RangeArgument = 
  RangeArgument {
    rangeArgumentMin :: GenericLiteralArgument,
    rangeArgumentMax :: GenericLiteralArgument}
  deriving (Eq, Ord, Read, Show)

_RangeArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument")

_RangeArgument_min = (Core.Name "min")

_RangeArgument_max = (Core.Name "max")

data WithOptionKeys = 
  WithOptionKeysShortestPath ShortestPathConstants |
  WithOptionKeysConnectedComponent ConnectedComponentConstants |
  WithOptionKeysPageRank PageRankConstants |
  WithOptionKeysPeerPressure PeerPressureConstants |
  WithOptionKeysIo IoOptionsKeys |
  WithOptionKeysWithOptionsTokens  |
  WithOptionKeysWithOptionsIndexer 
  deriving (Eq, Ord, Read, Show)

_WithOptionKeys = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys")

_WithOptionKeys_shortestPath = (Core.Name "shortestPath")

_WithOptionKeys_connectedComponent = (Core.Name "connectedComponent")

_WithOptionKeys_pageRank = (Core.Name "pageRank")

_WithOptionKeys_peerPressure = (Core.Name "peerPressure")

_WithOptionKeys_io = (Core.Name "io")

_WithOptionKeys_withOptionsTokens = (Core.Name "withOptionsTokens")

_WithOptionKeys_withOptionsIndexer = (Core.Name "withOptionsIndexer")

data ConnectedComponentConstants = 
  ConnectedComponentConstantsComponent  |
  ConnectedComponentConstantsEdges  |
  ConnectedComponentConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_ConnectedComponentConstants = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ConnectedComponentConstants")

_ConnectedComponentConstants_component = (Core.Name "component")

_ConnectedComponentConstants_edges = (Core.Name "edges")

_ConnectedComponentConstants_propertyName = (Core.Name "propertyName")

data PageRankConstants = 
  PageRankConstantsEdges  |
  PageRankConstantsTimes  |
  PageRankConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_PageRankConstants = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.PageRankConstants")

_PageRankConstants_edges = (Core.Name "edges")

_PageRankConstants_times = (Core.Name "times")

_PageRankConstants_propertyName = (Core.Name "propertyName")

data PeerPressureConstants = 
  PeerPressureConstantsEdges  |
  PeerPressureConstantsTimes  |
  PeerPressureConstantsPropertyName 
  deriving (Eq, Ord, Read, Show)

_PeerPressureConstants = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.PeerPressureConstants")

_PeerPressureConstants_edges = (Core.Name "edges")

_PeerPressureConstants_times = (Core.Name "times")

_PeerPressureConstants_propertyName = (Core.Name "propertyName")

data ShortestPathConstants = 
  ShortestPathConstantsTarget  |
  ShortestPathConstantsEdges  |
  ShortestPathConstantsDistance  |
  ShortestPathConstantsMaxDistance  |
  ShortestPathConstantsIncludeEdges 
  deriving (Eq, Ord, Read, Show)

_ShortestPathConstants = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants")

_ShortestPathConstants_target = (Core.Name "target")

_ShortestPathConstants_edges = (Core.Name "edges")

_ShortestPathConstants_distance = (Core.Name "distance")

_ShortestPathConstants_maxDistance = (Core.Name "maxDistance")

_ShortestPathConstants_includeEdges = (Core.Name "includeEdges")

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

_WithOptionsValues = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.WithOptionsValues")

_WithOptionsValues_tokens = (Core.Name "tokens")

_WithOptionsValues_none = (Core.Name "none")

_WithOptionsValues_ids = (Core.Name "ids")

_WithOptionsValues_labels = (Core.Name "labels")

_WithOptionsValues_keys = (Core.Name "keys")

_WithOptionsValues_values = (Core.Name "values")

_WithOptionsValues_all = (Core.Name "all")

_WithOptionsValues_list = (Core.Name "list")

_WithOptionsValues_map = (Core.Name "map")

data IoOptionsKeys = 
  IoOptionsKeysReader  |
  IoOptionsKeysWriter 
  deriving (Eq, Ord, Read, Show)

_IoOptionsKeys = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsKeys")

_IoOptionsKeys_reader = (Core.Name "reader")

_IoOptionsKeys_writer = (Core.Name "writer")

data IoOptionsValues = 
  IoOptionsValuesGryo  |
  IoOptionsValuesGraphson  |
  IoOptionsValuesGraphml 
  deriving (Eq, Ord, Read, Show)

_IoOptionsValues = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsValues")

_IoOptionsValues_gryo = (Core.Name "gryo")

_IoOptionsValues_graphson = (Core.Name "graphson")

_IoOptionsValues_graphml = (Core.Name "graphml")

data BooleanArgument = 
  BooleanArgumentValue Bool |
  BooleanArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_BooleanArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.BooleanArgument")

_BooleanArgument_value = (Core.Name "value")

_BooleanArgument_variable = (Core.Name "variable")

data IntegerArgument = 
  IntegerArgumentValue IntegerLiteral |
  IntegerArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_IntegerArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument")

_IntegerArgument_value = (Core.Name "value")

_IntegerArgument_variable = (Core.Name "variable")

data FloatArgument = 
  FloatArgumentValue FloatLiteral |
  FloatArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_FloatArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.FloatArgument")

_FloatArgument_value = (Core.Name "value")

_FloatArgument_variable = (Core.Name "variable")

data StringArgument = 
  StringArgumentValue String |
  StringArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StringArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringArgument")

_StringArgument_value = (Core.Name "value")

_StringArgument_variable = (Core.Name "variable")

data StringNullableArgument = 
  StringNullableArgumentValue (Maybe String) |
  StringNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StringNullableArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument")

_StringNullableArgument_value = (Core.Name "value")

_StringNullableArgument_variable = (Core.Name "variable")

data DateArgument = 
  DateArgumentValue DateLiteral |
  DateArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_DateArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.DateArgument")

_DateArgument_value = (Core.Name "value")

_DateArgument_variable = (Core.Name "variable")

data GenericLiteralArgument = 
  GenericLiteralArgumentValue GenericLiteral |
  GenericLiteralArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument")

_GenericLiteralArgument_value = (Core.Name "value")

_GenericLiteralArgument_variable = (Core.Name "variable")

data GenericLiteralListArgument = 
  GenericLiteralListArgumentValue GenericLiteralList |
  GenericLiteralListArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralListArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralListArgument")

_GenericLiteralListArgument_value = (Core.Name "value")

_GenericLiteralListArgument_variable = (Core.Name "variable")

data GenericLiteralMapArgument = 
  GenericLiteralMapArgumentValue GenericLiteralMap |
  GenericLiteralMapArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapArgument")

_GenericLiteralMapArgument_value = (Core.Name "value")

_GenericLiteralMapArgument_variable = (Core.Name "variable")

data GenericLiteralMapNullableArgument = 
  GenericLiteralMapNullableArgumentValue (Maybe GenericLiteralMap) |
  GenericLiteralMapNullableArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMapNullableArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument")

_GenericLiteralMapNullableArgument_value = (Core.Name "value")

_GenericLiteralMapNullableArgument_variable = (Core.Name "variable")

data StructureVertexArgument = 
  StructureVertexArgumentValue StructureVertex |
  StructureVertexArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_StructureVertexArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StructureVertexArgument")

_StructureVertexArgument_value = (Core.Name "value")

_StructureVertexArgument_variable = (Core.Name "variable")

data TraversalCardinalityArgument = 
  TraversalCardinalityArgumentValue TraversalCardinality |
  TraversalCardinalityArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalCardinalityArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgument")

_TraversalCardinalityArgument_value = (Core.Name "value")

_TraversalCardinalityArgument_variable = (Core.Name "variable")

data TraversalColumnArgument = 
  TraversalColumnArgumentValue TraversalColumn |
  TraversalColumnArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalColumnArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalColumnArgument")

_TraversalColumnArgument_value = (Core.Name "value")

_TraversalColumnArgument_variable = (Core.Name "variable")

data TraversalDirectionArgument = 
  TraversalDirectionArgumentValue TraversalDirection |
  TraversalDirectionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalDirectionArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirectionArgument")

_TraversalDirectionArgument_value = (Core.Name "value")

_TraversalDirectionArgument_variable = (Core.Name "variable")

data TraversalMergeArgument = 
  TraversalMergeArgumentValue TraversalMerge |
  TraversalMergeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalMergeArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument")

_TraversalMergeArgument_value = (Core.Name "value")

_TraversalMergeArgument_variable = (Core.Name "variable")

data TraversalOrderArgument = 
  TraversalOrderArgumentValue TraversalOrder |
  TraversalOrderArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalOrderArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalOrderArgument")

_TraversalOrderArgument_value = (Core.Name "value")

_TraversalOrderArgument_variable = (Core.Name "variable")

data TraversalPopArgument = 
  TraversalPopArgumentValue TraversalPop |
  TraversalPopArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalPopArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument")

_TraversalPopArgument_value = (Core.Name "value")

_TraversalPopArgument_variable = (Core.Name "variable")

data TraversalSackMethodArgument = 
  TraversalSackMethodArgumentValue  |
  TraversalSackMethodArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalSackMethodArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgument")

_TraversalSackMethodArgument_value = (Core.Name "value")

_TraversalSackMethodArgument_variable = (Core.Name "variable")

data TraversalScopeArgument = 
  TraversalScopeArgumentValue TraversalScope |
  TraversalScopeArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalScopeArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument")

_TraversalScopeArgument_value = (Core.Name "value")

_TraversalScopeArgument_variable = (Core.Name "variable")

data TraversalTokenArgument = 
  TraversalTokenArgumentValue TraversalToken |
  TraversalTokenArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalTokenArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument")

_TraversalTokenArgument_value = (Core.Name "value")

_TraversalTokenArgument_variable = (Core.Name "variable")

data TraversalComparatorArgument = 
  TraversalComparatorArgumentValue TraversalOrder |
  TraversalComparatorArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalComparatorArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalComparatorArgument")

_TraversalComparatorArgument_value = (Core.Name "value")

_TraversalComparatorArgument_variable = (Core.Name "variable")

data TraversalFunctionArgument = 
  TraversalFunctionArgumentValue TraversalFunction |
  TraversalFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalFunctionArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgument")

_TraversalFunctionArgument_value = (Core.Name "value")

_TraversalFunctionArgument_variable = (Core.Name "variable")

data TraversalBiFunctionArgument = 
  TraversalBiFunctionArgumentValue TraversalOperator |
  TraversalBiFunctionArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalBiFunctionArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument")

_TraversalBiFunctionArgument_value = (Core.Name "value")

_TraversalBiFunctionArgument_variable = (Core.Name "variable")

data TraversalDTArgument = 
  TraversalDTArgumentValue TraversalDT |
  TraversalDTArgumentVariable Identifier
  deriving (Eq, Ord, Read, Show)

_TraversalDTArgument = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.TraversalDTArgument")

_TraversalDTArgument_value = (Core.Name "value")

_TraversalDTArgument_variable = (Core.Name "variable")

newtype GenericLiteralList = 
  GenericLiteralList {
    unGenericLiteralList :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralList = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralList")

data GenericLiteralRange = 
  GenericLiteralRangeInteger IntegerRange |
  GenericLiteralRangeString StringRange
  deriving (Eq, Ord, Read, Show)

_GenericLiteralRange = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralRange")

_GenericLiteralRange_integer = (Core.Name "integer")

_GenericLiteralRange_string = (Core.Name "string")

data IntegerRange = 
  IntegerRange {
    integerRangeLeft :: IntegerLiteral,
    integerRangeRight :: IntegerLiteral}
  deriving (Eq, Ord, Read, Show)

_IntegerRange = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.IntegerRange")

_IntegerRange_left = (Core.Name "left")

_IntegerRange_right = (Core.Name "right")

data StringRange = 
  StringRange {
    stringRangeLeft :: String,
    stringRangeRight :: String}
  deriving (Eq, Ord, Read, Show)

_StringRange = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.StringRange")

_StringRange_left = (Core.Name "left")

_StringRange_right = (Core.Name "right")

newtype GenericLiteralSet = 
  GenericLiteralSet {
    unGenericLiteralSet :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralSet = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralSet")

newtype GenericLiteralCollection = 
  GenericLiteralCollection {
    unGenericLiteralCollection :: [GenericLiteral]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralCollection = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralCollection")

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

_GenericLiteral = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral")

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

newtype GenericLiteralMap = 
  GenericLiteralMap {
    unGenericLiteralMap :: [MapEntry]}
  deriving (Eq, Ord, Read, Show)

_GenericLiteralMap = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMap")

data MapEntry = 
  MapEntryKey MapKey |
  MapEntryValue GenericLiteral
  deriving (Eq, Ord, Read, Show)

_MapEntry = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.MapEntry")

_MapEntry_key = (Core.Name "key")

_MapEntry_value = (Core.Name "value")

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

_MapKey = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.MapKey")

_MapKey_string = (Core.Name "string")

_MapKey_numeric = (Core.Name "numeric")

_MapKey_traversalToken = (Core.Name "traversalToken")

_MapKey_traversalDirection = (Core.Name "traversalDirection")

_MapKey_set = (Core.Name "set")

_MapKey_collection = (Core.Name "collection")

_MapKey_map = (Core.Name "map")

_MapKey_keyword = (Core.Name "keyword")

_MapKey_identifier = (Core.Name "identifier")

newtype IntegerLiteral = 
  IntegerLiteral {
    unIntegerLiteral :: Integer}
  deriving (Eq, Ord, Read, Show)

_IntegerLiteral = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral")

newtype FloatLiteral = 
  FloatLiteral {
    unFloatLiteral :: Double}
  deriving (Eq, Ord, Read, Show)

_FloatLiteral = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.FloatLiteral")

data NumericLiteral = 
  NumericLiteralInteger IntegerLiteral |
  NumericLiteralFloat FloatLiteral
  deriving (Eq, Ord, Read, Show)

_NumericLiteral = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral")

_NumericLiteral_integer = (Core.Name "integer")

_NumericLiteral_float = (Core.Name "float")

newtype DateLiteral = 
  DateLiteral {
    unDateLiteral :: (Maybe StringArgument)}
  deriving (Eq, Ord, Read, Show)

_DateLiteral = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.DateLiteral")

data Keyword = 
  KeywordEdges  |
  KeywordKeys  |
  KeywordNew  |
  KeywordValues 
  deriving (Eq, Ord, Read, Show)

_Keyword = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.Keyword")

_Keyword_edges = (Core.Name "edges")

_Keyword_keys = (Core.Name "keys")

_Keyword_new = (Core.Name "new")

_Keyword_values = (Core.Name "values")

newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra.ext.org.apache.tinkerpop.gremlin.Identifier")
