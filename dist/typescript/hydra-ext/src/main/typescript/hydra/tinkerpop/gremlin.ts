// Note: this is an automatically generated file. Do not edit.

/**
 * A Gremlin model, based on the Gremlin ANTLR grammar (master branch, as of 2024-06-30).
 */



import * as Core from "../core.js";

export type QueryList = ReadonlyArray<Query> & { readonly __brand: "QueryList" };

export type Query =
  | { readonly tag: "traversalSource"; readonly value: TraversalSourceQuery }
  | { readonly tag: "rootTraversal"; readonly value: RootTraversalQuery }
  | { readonly tag: "toString" }
  | { readonly tag: "empty" };

export interface TraversalSourceQuery {
  readonly source: TraversalSource;
  readonly transactionPart: TransactionPart | null;
}

export interface RootTraversalQuery {
  readonly root: RootTraversal;
  readonly terminalMethod: TraversalTerminalMethod | null;
}

export type TraversalSource = ReadonlyArray<TraversalSourceSelfMethod> & { readonly __brand: "TraversalSource" };

export type TransactionPart =
  | { readonly tag: "begin" }
  | { readonly tag: "commit" }
  | { readonly tag: "rollback" };

export interface RootTraversal {
  readonly source: TraversalSource;
  readonly spawnMethod: TraversalSourceSpawnMethod;
  readonly chained: ReadonlyArray<ChainedTraversalElement>;
}

export type TraversalSourceSelfMethod =
  | { readonly tag: "withBulk"; readonly value: boolean }
  | { readonly tag: "withPath" }
  | { readonly tag: "withSack"; readonly value: GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument }
  | { readonly tag: "withSideEffect"; readonly value: StringArgumentAndGenericLiteralArgument }
  | { readonly tag: "withStrategies"; readonly value: ReadonlyArray<TraversalStrategy> }
  | { readonly tag: "withoutStrategies"; readonly value: ReadonlyArray<Identifier> }
  | { readonly tag: "with"; readonly value: StringArgumentAndOptionalGenericLiteralArgument };

export interface GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument {
  readonly literal: GenericLiteralArgument;
  readonly biFunction: TraversalBiFunctionArgument | null;
}

export interface StringArgumentAndGenericLiteralArgument {
  readonly string: StringArgument;
  readonly literal: GenericLiteralArgument;
}

export interface StringArgumentAndOptionalGenericLiteralArgument {
  readonly string: StringArgument;
  readonly literal: GenericLiteralArgument | null;
}

export type TraversalSourceSpawnMethod =
  | { readonly tag: "addE"; readonly value: StringArgumentOrNestedTraversal }
  | { readonly tag: "addV"; readonly value: StringArgumentOrNestedTraversal | null }
  | { readonly tag: "e"; readonly value: ReadonlyArray<GenericLiteralArgument> }
  | { readonly tag: "v"; readonly value: ReadonlyArray<GenericLiteralArgument> }
  | { readonly tag: "mergeV"; readonly value: GenericLiteralMapNullableArgumentOrNestedTraversal }
  | { readonly tag: "mergeE"; readonly value: GenericLiteralMapNullableArgumentOrNestedTraversal }
  | { readonly tag: "inject"; readonly value: ReadonlyArray<GenericLiteralArgument> }
  | { readonly tag: "io"; readonly value: StringArgument }
  | { readonly tag: "call"; readonly value: ServiceCall | null }
  | { readonly tag: "union"; readonly value: ReadonlyArray<NestedTraversal> };

export type GenericLiteralMapNullableArgumentOrNestedTraversal =
  | { readonly tag: "map"; readonly value: GenericLiteralMapNullableArgument }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface ServiceCall {
  readonly service: StringArgument;
  readonly arguments: ServiceArguments;
}

export type ServiceArguments =
  | { readonly tag: "map"; readonly value: GenericLiteralMapArgument | null }
  | { readonly tag: "traversal"; readonly value: NestedTraversal | null };

export interface ChainedTraversal {
  readonly first: TraversalMethod;
  readonly rest: ChainedTraversalElement;
}

export type ChainedTraversalElement =
  | { readonly tag: "method"; readonly value: TraversalMethod }
  | { readonly tag: "self"; readonly value: TraversalSelfMethod };

export type NestedTraversal =
  | { readonly tag: "root"; readonly value: RootTraversal }
  | { readonly tag: "chained"; readonly value: ChainedTraversal }
  | { readonly tag: "anonymous"; readonly value: ChainedTraversal };

export interface TerminatedTraversal {
  readonly root: RootTraversal;
  readonly terminal: TraversalTerminalMethod;
}

export type TraversalMethod =
  | { readonly tag: "v"; readonly value: ReadonlyArray<GenericLiteralArgument> }
  | { readonly tag: "e"; readonly value: ReadonlyArray<GenericLiteralArgument> }
  | { readonly tag: "addE"; readonly value: StringArgumentOrNestedTraversal }
  | { readonly tag: "addV"; readonly value: StringArgumentOrNestedTraversal | null }
  | { readonly tag: "mergeE"; readonly value: GenericLiteralMapNullableArgumentOrNestedTraversal | null }
  | { readonly tag: "mergeV"; readonly value: GenericLiteralMapNullableArgumentOrNestedTraversal | null }
  | { readonly tag: "aggregate"; readonly value: OptionalTraversalScopeArgumentAndStringArgument }
  | { readonly tag: "all"; readonly value: TraversalPredicate }
  | { readonly tag: "and"; readonly value: ReadonlyArray<NestedTraversal> }
  | { readonly tag: "any"; readonly value: TraversalPredicate }
  | { readonly tag: "as"; readonly value: StringArgumentAndOptionalStringLiteralVarargs }
  | { readonly tag: "barrier"; readonly value: TraversalSackMethodArgumentOrIntegerArgument | null }
  | { readonly tag: "both"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "bothE"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "bothV" }
  | { readonly tag: "branch"; readonly value: NestedTraversal }
  | { readonly tag: "by"; readonly value: ByArgs }
  | { readonly tag: "cap"; readonly value: StringArgumentAndOptionalStringLiteralVarargs }
  | { readonly tag: "choose"; readonly value: ChooseArgs }
  | { readonly tag: "coalesce"; readonly value: ReadonlyArray<NestedTraversal> }
  | { readonly tag: "coin"; readonly value: FloatArgument }
  | { readonly tag: "conjoin"; readonly value: StringArgument }
  | { readonly tag: "connectedComponent" }
  | { readonly tag: "constant"; readonly value: GenericLiteralArgument }
  | { readonly tag: "count"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "cyclicPath" }
  | { readonly tag: "dedup"; readonly value: DedupArgs }
  | { readonly tag: "difference"; readonly value: GenericLiteralArgument }
  | { readonly tag: "disjunct"; readonly value: GenericLiteralArgument }
  | { readonly tag: "drop" }
  | { readonly tag: "elementMap"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "emit"; readonly value: PredicateOrTraversal | null }
  | { readonly tag: "filter"; readonly value: PredicateOrTraversal }
  | { readonly tag: "flatMap"; readonly value: NestedTraversal }
  | { readonly tag: "fold"; readonly value: GenericLiteralArgumentAndTraversalBiFunctionArgument | null }
  | { readonly tag: "from"; readonly value: FromArgs }
  | { readonly tag: "group"; readonly value: StringArgument | null }
  | { readonly tag: "groupCount"; readonly value: StringArgument | null }
  | { readonly tag: "has"; readonly value: HasArgs }
  | { readonly tag: "hasId"; readonly value: GenericLiteralArgumentAndTraversalPredicate }
  | { readonly tag: "hasKey"; readonly value: TraversalPredicateOrStringLiteralVarargs }
  | { readonly tag: "hasLabel"; readonly value: TraversalPredicateOrStringLiteralVarargs }
  | { readonly tag: "hasNot"; readonly value: StringNullableArgument }
  | { readonly tag: "hasValue"; readonly value: TraversalPredicateOrGenericLiteralArgument }
  | { readonly tag: "id" }
  | { readonly tag: "identity" }
  | { readonly tag: "in"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "inE"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "intersect"; readonly value: GenericLiteralArgument }
  | { readonly tag: "inV" }
  | { readonly tag: "index" }
  | { readonly tag: "inject"; readonly value: ReadonlyArray<GenericLiteralArgument> }
  | { readonly tag: "is"; readonly value: TraversalPredicateOrGenericLiteralArgument }
  | { readonly tag: "key" }
  | { readonly tag: "label" }
  | { readonly tag: "limit"; readonly value: OptionalTraversalScopeArgumentAndIntegerArgument }
  | { readonly tag: "local"; readonly value: NestedTraversal }
  | { readonly tag: "loops"; readonly value: StringArgument | null }
  | { readonly tag: "map"; readonly value: NestedTraversal }
  | { readonly tag: "match"; readonly value: ReadonlyArray<NestedTraversal> }
  | { readonly tag: "math"; readonly value: StringArgument }
  | { readonly tag: "max"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "mean"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "min"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "none"; readonly value: TraversalPredicate }
  | { readonly tag: "not"; readonly value: NestedTraversal }
  | { readonly tag: "option"; readonly value: OptionArgs }
  | { readonly tag: "optional"; readonly value: NestedTraversal }
  | { readonly tag: "or"; readonly value: ReadonlyArray<NestedTraversal> }
  | { readonly tag: "order"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "otherV" }
  | { readonly tag: "out"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "outE"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "outV" }
  | { readonly tag: "pageRank"; readonly value: FloatArgument | null }
  | { readonly tag: "path" }
  | { readonly tag: "peerPressure" }
  | { readonly tag: "profile"; readonly value: StringArgument | null }
  | { readonly tag: "project"; readonly value: StringArgumentAndOptionalStringLiteralVarargs }
  | { readonly tag: "properties"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "property"; readonly value: PropertyArgs }
  | { readonly tag: "propertyMap"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "range"; readonly value: RangeArgs }
  | { readonly tag: "read" }
  | { readonly tag: "repeat"; readonly value: OptionalStringArgumentAndNestedTraversal }
  | { readonly tag: "sack"; readonly value: TraversalBiFunctionArgument | null }
  | { readonly tag: "sample"; readonly value: OptionalTraversalScopeArgumentAndIntegerArgument }
  | { readonly tag: "select"; readonly value: SelectArgs }
  | { readonly tag: "combine"; readonly value: GenericLiteralArgument }
  | { readonly tag: "product"; readonly value: GenericLiteralArgument }
  | { readonly tag: "merge"; readonly value: GenericLiteralArgument }
  | { readonly tag: "shortestPath" }
  | { readonly tag: "sideEffect"; readonly value: NestedTraversal }
  | { readonly tag: "simplePath" }
  | { readonly tag: "skip"; readonly value: OptionalTraversalScopeArgumentAndIntegerArgument }
  | { readonly tag: "store"; readonly value: StringArgument }
  | { readonly tag: "subgraph"; readonly value: StringArgument }
  | { readonly tag: "sum"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "tail"; readonly value: TailArgs | null }
  | { readonly tag: "fail"; readonly value: StringArgument | null }
  | { readonly tag: "times"; readonly value: IntegerArgument }
  | { readonly tag: "to"; readonly value: ToArgs }
  | { readonly tag: "toE"; readonly value: DirectionAndVarargs }
  | { readonly tag: "toV"; readonly value: TraversalDirectionArgument }
  | { readonly tag: "tree"; readonly value: StringArgument | null }
  | { readonly tag: "unfold" }
  | { readonly tag: "union"; readonly value: ReadonlyArray<NestedTraversal> }
  | { readonly tag: "until"; readonly value: PredicateOrTraversal }
  | { readonly tag: "value" }
  | { readonly tag: "valueMap"; readonly value: ValueMapArgs }
  | { readonly tag: "values"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "where"; readonly value: WhereArgs }
  | { readonly tag: "with"; readonly value: WithArgs }
  | { readonly tag: "write" }
  | { readonly tag: "element"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "call"; readonly value: ServiceCall }
  | { readonly tag: "concat"; readonly value: ConcatArgs }
  | { readonly tag: "asString"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "format"; readonly value: StringArgument }
  | { readonly tag: "toUpper"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "toLower"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "length"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "trim"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "lTrim"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "rTrim"; readonly value: TraversalScopeArgument | null }
  | { readonly tag: "reverse" }
  | { readonly tag: "replace"; readonly value: ReplaceArgs }
  | { readonly tag: "split"; readonly value: SplitArgs }
  | { readonly tag: "substring"; readonly value: SubstringArgs }
  | { readonly tag: "asDate" }
  | { readonly tag: "dateAdd"; readonly value: DateAddArgs }
  | { readonly tag: "dateDiff"; readonly value: DateDiffArgs };

export type StringArgumentOrNestedTraversal =
  | { readonly tag: "string"; readonly value: StringArgument }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface OptionalTraversalScopeArgumentAndStringArgument {
  readonly scope: TraversalScopeArgument | null;
  readonly string: StringArgument;
}

export interface StringArgumentAndOptionalStringLiteralVarargs {
  readonly first: StringArgument;
  readonly rest: ReadonlyArray<StringNullableArgument>;
}

export type TraversalSackMethodArgumentOrIntegerArgument =
  | { readonly tag: "consumer"; readonly value: TraversalSackMethodArgument }
  | { readonly tag: "int"; readonly value: IntegerArgument };

export type ByArgs =
  | { readonly tag: "order"; readonly value: TraversalOrderArgument }
  | { readonly tag: "token"; readonly value: TraversalTokenArgument }
  | { readonly tag: "other"; readonly value: ByOtherArgs };

export type ByOtherArgs =
  | { readonly tag: "comparator"; readonly value: TraversalComparatorArgument | null }
  | { readonly tag: "other"; readonly value: TraversalFunctionArgumentOrStringArgumentOrNestedTraversal | null };

export type TraversalFunctionArgumentOrStringArgumentOrNestedTraversal =
  | { readonly tag: "function"; readonly value: TraversalFunctionArgument }
  | { readonly tag: "string"; readonly value: StringArgument }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export type ChooseArgs =
  | { readonly tag: "function"; readonly value: TraversalFunctionArgument }
  | { readonly tag: "predicateTraversal"; readonly value: PredicateTraversalArgument }
  | { readonly tag: "traversal"; readonly value: NestedTraversalArgument };

export interface PredicateTraversalArgument {
  readonly predicate: TraversalPredicate;
  readonly traversal1: NestedTraversal;
  readonly traversal2: NestedTraversal | null;
}

export interface NestedTraversalArgument {
  readonly traversal1: NestedTraversal;
  readonly traversal2: NestedTraversal | null;
  readonly traversal3: NestedTraversal | null;
}

export type DedupArgs =
  | { readonly tag: "scopeString"; readonly value: ScopeStringArgument }
  | { readonly tag: "string"; readonly value: ReadonlyArray<StringNullableArgument> };

export interface ScopeStringArgument {
  readonly scope: TraversalScopeArgument;
  readonly strings: ReadonlyArray<StringNullableArgument>;
}

export type PredicateOrTraversal =
  | { readonly tag: "predicate"; readonly value: TraversalPredicate }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface GenericLiteralArgumentAndTraversalBiFunctionArgument {
  readonly literal: GenericLiteralArgument;
  readonly biFunction: TraversalBiFunctionArgument;
}

export type FromArgs =
  | { readonly tag: "string"; readonly value: StringArgument }
  | { readonly tag: "vertex"; readonly value: StructureVertexArgument }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export type HasArgs =
  | { readonly tag: "string"; readonly value: HasStringArgumentAndOptionalStringLiteralVarargs }
  | { readonly tag: "traversalToken"; readonly value: HasTraversalTokenArgs };

export interface HasStringArgumentAndOptionalStringLiteralVarargs {
  readonly string: StringNullableArgument;
  readonly rest: HasStringArgumentAndOptionalStringLiteralVarargsRest | null;
}

export type HasStringArgumentAndOptionalStringLiteralVarargsRest =
  | { readonly tag: "object"; readonly value: GenericLiteralArgument }
  | { readonly tag: "predicate"; readonly value: TraversalPredicate }
  | { readonly tag: "stringObject"; readonly value: StringNullableArgumentAndGenericLiteralArgument }
  | { readonly tag: "stringPredicate"; readonly value: StringNullableArgumentAndTraversalPredicate }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface StringNullableArgumentAndGenericLiteralArgument {
  readonly string: StringNullableArgument;
  readonly literal: GenericLiteralArgument;
}

export interface StringNullableArgumentAndTraversalPredicate {
  readonly string: StringNullableArgument;
  readonly predicate: TraversalPredicate;
}

export interface HasTraversalTokenArgs {
  readonly traversalToken: TraversalTokenArgument;
  readonly rest: HasTraversalTokenArgsRest;
}

export type HasTraversalTokenArgsRest =
  | { readonly tag: "literal"; readonly value: GenericLiteralArgument }
  | { readonly tag: "predicate"; readonly value: TraversalPredicate }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export type GenericLiteralArgumentAndTraversalPredicate =
  | { readonly tag: "literal"; readonly value: GenericLiteralArgument }
  | { readonly tag: "predicate"; readonly value: TraversalPredicate };

export type TraversalPredicateOrStringLiteralVarargs =
  | { readonly tag: "predicate"; readonly value: TraversalPredicate }
  | { readonly tag: "string"; readonly value: ReadonlyArray<StringNullableArgument> };

export type TraversalPredicateOrGenericLiteralArgument =
  | { readonly tag: "predicate"; readonly value: TraversalPredicate }
  | { readonly tag: "literal"; readonly value: ReadonlyArray<GenericLiteralArgument> };

export type OptionArgs =
  | { readonly tag: "predicateTraversal"; readonly value: TraversalPredicateAndNestedTraversal }
  | { readonly tag: "mergeMap"; readonly value: TraversalMergeArgumentAndGenericLiteralMapNullableArgument }
  | { readonly tag: "mergeTraversal"; readonly value: TraversalMergeArgumentAndNestedTraversal }
  | { readonly tag: "objectTraversal"; readonly value: GenericLiteralArgumentAndNestedTraversal }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface TraversalPredicateAndNestedTraversal {
  readonly predicate: TraversalPredicate;
  readonly traversal: NestedTraversal;
}

export interface TraversalMergeArgumentAndGenericLiteralMapNullableArgument {
  readonly merge: TraversalMergeArgument;
  readonly map: GenericLiteralMapNullableArgument;
  readonly cardinality: TraversalCardinality | null;
}

export interface TraversalMergeArgumentAndNestedTraversal {
  readonly merge: TraversalMergeArgument;
  readonly traversal: NestedTraversal;
}

export interface GenericLiteralArgumentAndNestedTraversal {
  readonly object: GenericLiteralArgument;
  readonly traversal: NestedTraversal;
}

export type PropertyArgs =
  | { readonly tag: "cardinalityObjects"; readonly value: TraversalCardinalityArgumentAndObjects }
  | { readonly tag: "objects"; readonly value: ReadonlyArray<GenericLiteralArgument> }
  | { readonly tag: "object"; readonly value: GenericLiteralMapNullableArgument }
  | { readonly tag: "cardinalityObject"; readonly value: GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument };

export interface TraversalCardinalityArgumentAndObjects {
  readonly cardinality: TraversalCardinalityArgument;
  readonly objects: ReadonlyArray<GenericLiteralArgument>;
}

export interface GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument {
  readonly cardinality: TraversalCardinalityArgument;
  readonly object: GenericLiteralMapNullableArgument;
}

export interface RangeArgs {
  readonly scope: TraversalScopeArgument | null;
  readonly min: IntegerArgument;
  readonly max: IntegerArgument;
}

export interface OptionalStringArgumentAndNestedTraversal {
  readonly string: StringArgument | null;
  readonly traversal: NestedTraversal;
}

export interface OptionalTraversalScopeArgumentAndIntegerArgument {
  readonly scope: TraversalScopeArgument | null;
  readonly long: IntegerArgument;
}

export type SelectArgs =
  | { readonly tag: "column"; readonly value: TraversalColumnArgument }
  | { readonly tag: "popStrings"; readonly value: PopStringsArgument }
  | { readonly tag: "popTraversal"; readonly value: TraversalPopArgumentAndNestedTraversal }
  | { readonly tag: "strings"; readonly value: ReadonlyArray<StringArgument> }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface PopStringsArgument {
  readonly pop: TraversalPopArgument;
  readonly string: ReadonlyArray<StringArgument>;
}

export interface TraversalPopArgumentAndNestedTraversal {
  readonly pop: TraversalPopArgument;
  readonly traversal: NestedTraversal;
}

export interface TailArgs {
  readonly scope: TraversalScopeArgument | null;
  readonly integer: IntegerArgument | null;
}

export type ToArgs =
  | { readonly tag: "direction"; readonly value: DirectionAndVarargs }
  | { readonly tag: "string"; readonly value: StringArgument }
  | { readonly tag: "vertex"; readonly value: StructureVertexArgument }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface DirectionAndVarargs {
  readonly direction: TraversalDirectionArgument;
  readonly varargs: ReadonlyArray<StringNullableArgument>;
}

export type ValueMapArgs =
  | { readonly tag: "string"; readonly value: ReadonlyArray<StringNullableArgument> }
  | { readonly tag: "boolean"; readonly value: ValueMapBooleanArgs };

export interface ValueMapBooleanArgs {
  readonly value: BooleanArgument;
  readonly keys: ReadonlyArray<StringNullableArgument> | null;
}

export type WhereArgs =
  | { readonly tag: "predicate"; readonly value: WhereWithPredicateArgs }
  | { readonly tag: "string"; readonly value: StringArgument }
  | { readonly tag: "traversal"; readonly value: NestedTraversal };

export interface WhereWithPredicateArgs {
  readonly leftArg: StringArgument | null;
  readonly predicate: TraversalPredicate;
}

export interface WithArgs {
  readonly keys: WithArgsKeys;
  readonly values: WithArgsValues | null;
}

export type WithArgsKeys =
  | { readonly tag: "withOption"; readonly value: WithOptionKeys }
  | { readonly tag: "string"; readonly value: StringArgument };

export type WithArgsValues =
  | { readonly tag: "withOptions"; readonly value: WithOptionsValues }
  | { readonly tag: "io"; readonly value: IoOptionsValues }
  | { readonly tag: "object"; readonly value: GenericLiteralArgument };

export type ConcatArgs =
  | { readonly tag: "traversal"; readonly value: ReadonlyArray<NestedTraversal> }
  | { readonly tag: "string"; readonly value: ReadonlyArray<StringNullableArgument> };

export interface ReplaceArgs {
  readonly scope: TraversalScopeArgument | null;
  readonly from: StringNullableArgument;
  readonly to: StringNullableArgument;
}

export interface SplitArgs {
  readonly scope: TraversalScopeArgument | null;
  readonly delimiter: StringNullableArgument;
}

export interface SubstringArgs {
  readonly scope: TraversalScopeArgument | null;
  readonly start: IntegerArgument;
  readonly end: IntegerArgument | null;
}

export interface DateAddArgs {
  readonly unit: TraversalDTArgument;
  readonly duration: IntegerArgument;
}

export type DateDiffArgs =
  | { readonly tag: "traversal"; readonly value: NestedTraversal }
  | { readonly tag: "date"; readonly value: DateArgument };

export interface StructureVertex {
  readonly new: boolean;
  readonly id: GenericLiteralArgument;
  readonly label: StringArgument;
}

export interface TraversalStrategy {
  readonly new: boolean;
  readonly class: Identifier;
  readonly configurations: ReadonlyArray<Configuration>;
}

export interface Configuration {
  readonly key: KeywordOrIdentifier;
  readonly value: GenericLiteralArgument;
}

export type KeywordOrIdentifier =
  | { readonly tag: "keyword"; readonly value: Keyword }
  | { readonly tag: "identifier"; readonly value: Identifier };

export type TraversalScope =
  | { readonly tag: "local" }
  | { readonly tag: "global" };

export type TraversalToken =
  | { readonly tag: "id" }
  | { readonly tag: "label" }
  | { readonly tag: "key" }
  | { readonly tag: "value" };

export type TraversalMerge =
  | { readonly tag: "onCreate" }
  | { readonly tag: "onMatch" }
  | { readonly tag: "outV" }
  | { readonly tag: "inV" };

export type TraversalOrder =
  | { readonly tag: "incr" }
  | { readonly tag: "decr" }
  | { readonly tag: "asc" }
  | { readonly tag: "desc" }
  | { readonly tag: "shuffle" };

export type TraversalDirection =
  | { readonly tag: "in" }
  | { readonly tag: "out" }
  | { readonly tag: "both" };

export type TraversalCardinality =
  | { readonly tag: "single"; readonly value: GenericLiteral }
  | { readonly tag: "set"; readonly value: GenericLiteral }
  | { readonly tag: "list"; readonly value: GenericLiteral };

export type TraversalColumn =
  | { readonly tag: "keys" }
  | { readonly tag: "values" };

export type TraversalPop =
  | { readonly tag: "first" }
  | { readonly tag: "last" }
  | { readonly tag: "all" }
  | { readonly tag: "mixed" };

export type TraversalOperator =
  | { readonly tag: "addAll" }
  | { readonly tag: "and" }
  | { readonly tag: "assign" }
  | { readonly tag: "div" }
  | { readonly tag: "max" }
  | { readonly tag: "min" }
  | { readonly tag: "minus" }
  | { readonly tag: "mult" }
  | { readonly tag: "or" }
  | { readonly tag: "sum" }
  | { readonly tag: "sumLong" };

export type TraversalPick =
  | { readonly tag: "any" }
  | { readonly tag: "none" };

export type TraversalDT =
  | { readonly tag: "second" }
  | { readonly tag: "minute" }
  | { readonly tag: "hour" }
  | { readonly tag: "day" };

export type TraversalPredicate =
  | { readonly tag: "eq"; readonly value: GenericLiteralArgument }
  | { readonly tag: "neq"; readonly value: GenericLiteralArgument }
  | { readonly tag: "lt"; readonly value: GenericLiteralArgument }
  | { readonly tag: "lte"; readonly value: GenericLiteralArgument }
  | { readonly tag: "gt"; readonly value: GenericLiteralArgument }
  | { readonly tag: "gte"; readonly value: GenericLiteralArgument }
  | { readonly tag: "inside"; readonly value: RangeArgument }
  | { readonly tag: "outside"; readonly value: RangeArgument }
  | { readonly tag: "between"; readonly value: RangeArgument }
  | { readonly tag: "within"; readonly value: GenericLiteralArgument | null }
  | { readonly tag: "without"; readonly value: GenericLiteralArgument | null }
  | { readonly tag: "not"; readonly value: TraversalPredicate }
  | { readonly tag: "startingWith"; readonly value: StringArgument }
  | { readonly tag: "notStartingWith"; readonly value: StringArgument }
  | { readonly tag: "endingWith"; readonly value: StringArgument }
  | { readonly tag: "notEndingWith"; readonly value: StringArgument }
  | { readonly tag: "containing"; readonly value: StringArgument }
  | { readonly tag: "notContaining"; readonly value: StringArgument }
  | { readonly tag: "regex"; readonly value: StringArgument }
  | { readonly tag: "notRegex"; readonly value: StringArgument }
  | { readonly tag: "and"; readonly value: TwoTraversalPredicates }
  | { readonly tag: "or"; readonly value: TwoTraversalPredicates }
  | { readonly tag: "negate"; readonly value: TraversalPredicate };

export interface TwoTraversalPredicates {
  readonly left: TraversalPredicate;
  readonly right: TraversalPredicate;
}

export type TraversalTerminalMethod =
  | { readonly tag: "explain" }
  | { readonly tag: "iterate" }
  | { readonly tag: "hasNext" }
  | { readonly tag: "tryNext" }
  | { readonly tag: "next"; readonly value: IntegerLiteral | null }
  | { readonly tag: "toList" }
  | { readonly tag: "toSet" }
  | { readonly tag: "toBulkSet" };

export type TraversalSelfMethod =
  | { readonly tag: "discard" };

export type TraversalFunction =
  | { readonly tag: "token"; readonly value: TraversalToken }
  | { readonly tag: "column"; readonly value: TraversalColumn };

export interface RangeArgument {
  readonly min: GenericLiteralArgument;
  readonly max: GenericLiteralArgument;
}

export type WithOptionKeys =
  | { readonly tag: "shortestPath"; readonly value: ShortestPathConstants }
  | { readonly tag: "connectedComponent"; readonly value: ConnectedComponentConstants }
  | { readonly tag: "pageRank"; readonly value: PageRankConstants }
  | { readonly tag: "peerPressure"; readonly value: PeerPressureConstants }
  | { readonly tag: "io"; readonly value: IoOptionsKeys }
  | { readonly tag: "withOptionsTokens" }
  | { readonly tag: "withOptionsIndexer" };

export type ConnectedComponentConstants =
  | { readonly tag: "component" }
  | { readonly tag: "edges" }
  | { readonly tag: "propertyName" };

export type PageRankConstants =
  | { readonly tag: "edges" }
  | { readonly tag: "times" }
  | { readonly tag: "propertyName" };

export type PeerPressureConstants =
  | { readonly tag: "edges" }
  | { readonly tag: "times" }
  | { readonly tag: "propertyName" };

export type ShortestPathConstants =
  | { readonly tag: "target" }
  | { readonly tag: "edges" }
  | { readonly tag: "distance" }
  | { readonly tag: "maxDistance" }
  | { readonly tag: "includeEdges" };

export type WithOptionsValues =
  | { readonly tag: "tokens" }
  | { readonly tag: "none" }
  | { readonly tag: "ids" }
  | { readonly tag: "labels" }
  | { readonly tag: "keys" }
  | { readonly tag: "values" }
  | { readonly tag: "all" }
  | { readonly tag: "list" }
  | { readonly tag: "map" };

export type IoOptionsKeys =
  | { readonly tag: "reader" }
  | { readonly tag: "writer" };

export type IoOptionsValues =
  | { readonly tag: "gryo" }
  | { readonly tag: "graphson" }
  | { readonly tag: "graphml" };

export type BooleanArgument =
  | { readonly tag: "value"; readonly value: boolean }
  | { readonly tag: "variable"; readonly value: Identifier };

export type IntegerArgument =
  | { readonly tag: "value"; readonly value: IntegerLiteral }
  | { readonly tag: "variable"; readonly value: Identifier };

export type FloatArgument =
  | { readonly tag: "value"; readonly value: FloatLiteral }
  | { readonly tag: "variable"; readonly value: Identifier };

export type StringArgument =
  | { readonly tag: "value"; readonly value: string }
  | { readonly tag: "variable"; readonly value: Identifier };

export type StringNullableArgument =
  | { readonly tag: "value"; readonly value: string | null }
  | { readonly tag: "variable"; readonly value: Identifier };

export type DateArgument =
  | { readonly tag: "value"; readonly value: DateLiteral }
  | { readonly tag: "variable"; readonly value: Identifier };

export type GenericLiteralArgument =
  | { readonly tag: "value"; readonly value: GenericLiteral }
  | { readonly tag: "variable"; readonly value: Identifier };

export type GenericLiteralListArgument =
  | { readonly tag: "value"; readonly value: GenericLiteralList }
  | { readonly tag: "variable"; readonly value: Identifier };

export type GenericLiteralMapArgument =
  | { readonly tag: "value"; readonly value: GenericLiteralMap }
  | { readonly tag: "variable"; readonly value: Identifier };

export type GenericLiteralMapNullableArgument =
  | { readonly tag: "value"; readonly value: GenericLiteralMap | null }
  | { readonly tag: "variable"; readonly value: Identifier };

export type StructureVertexArgument =
  | { readonly tag: "value"; readonly value: StructureVertex }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalCardinalityArgument =
  | { readonly tag: "value"; readonly value: TraversalCardinality }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalColumnArgument =
  | { readonly tag: "value"; readonly value: TraversalColumn }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalDirectionArgument =
  | { readonly tag: "value"; readonly value: TraversalDirection }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalMergeArgument =
  | { readonly tag: "value"; readonly value: TraversalMerge }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalOrderArgument =
  | { readonly tag: "value"; readonly value: TraversalOrder }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalPopArgument =
  | { readonly tag: "value"; readonly value: TraversalPop }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalSackMethodArgument =
  | { readonly tag: "value" }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalScopeArgument =
  | { readonly tag: "value"; readonly value: TraversalScope }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalTokenArgument =
  | { readonly tag: "value"; readonly value: TraversalToken }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalComparatorArgument =
  | { readonly tag: "value"; readonly value: TraversalOrder }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalFunctionArgument =
  | { readonly tag: "value"; readonly value: TraversalFunction }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalBiFunctionArgument =
  | { readonly tag: "value"; readonly value: TraversalOperator }
  | { readonly tag: "variable"; readonly value: Identifier };

export type TraversalDTArgument =
  | { readonly tag: "value"; readonly value: TraversalDT }
  | { readonly tag: "variable"; readonly value: Identifier };

export type GenericLiteralList = ReadonlyArray<GenericLiteral> & { readonly __brand: "GenericLiteralList" };

export type GenericLiteralRange =
  | { readonly tag: "integer"; readonly value: IntegerRange }
  | { readonly tag: "string"; readonly value: StringRange };

export interface IntegerRange {
  readonly left: IntegerLiteral;
  readonly right: IntegerLiteral;
}

export interface StringRange {
  readonly left: string;
  readonly right: string;
}

export type GenericLiteralSet = ReadonlyArray<GenericLiteral> & { readonly __brand: "GenericLiteralSet" };

export type GenericLiteralCollection = ReadonlyArray<GenericLiteral> & { readonly __brand: "GenericLiteralCollection" };

export type GenericLiteral =
  | { readonly tag: "numeric"; readonly value: NumericLiteral }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "string"; readonly value: string }
  | { readonly tag: "date"; readonly value: DateLiteral }
  | { readonly tag: "null" }
  | { readonly tag: "nan" }
  | { readonly tag: "inf" }
  | { readonly tag: "traversalToken"; readonly value: TraversalToken }
  | { readonly tag: "traversalCardinality"; readonly value: TraversalCardinality }
  | { readonly tag: "traversalDirection"; readonly value: TraversalDirection }
  | { readonly tag: "traversalMerge"; readonly value: TraversalMerge }
  | { readonly tag: "traversalPick"; readonly value: TraversalPick }
  | { readonly tag: "traversalDT"; readonly value: TraversalDT }
  | { readonly tag: "structureVertex"; readonly value: StructureVertex }
  | { readonly tag: "genericLiteralSet"; readonly value: GenericLiteralSet }
  | { readonly tag: "genericLiteralCollection"; readonly value: GenericLiteralCollection }
  | { readonly tag: "genericLiteralRange"; readonly value: GenericLiteralRange }
  | { readonly tag: "nestedTraversal"; readonly value: NestedTraversal }
  | { readonly tag: "terminatedTraversal"; readonly value: TerminatedTraversal }
  | { readonly tag: "genericLiteralMap"; readonly value: GenericLiteralMap };

export type GenericLiteralMap = ReadonlyArray<MapEntry> & { readonly __brand: "GenericLiteralMap" };

export type MapEntry =
  | { readonly tag: "key"; readonly value: MapKey }
  | { readonly tag: "value"; readonly value: GenericLiteral };

export type MapKey =
  | { readonly tag: "string"; readonly value: string }
  | { readonly tag: "numeric"; readonly value: NumericLiteral }
  | { readonly tag: "traversalToken"; readonly value: TraversalToken }
  | { readonly tag: "traversalDirection"; readonly value: TraversalDirection }
  | { readonly tag: "set"; readonly value: GenericLiteralSet }
  | { readonly tag: "collection"; readonly value: GenericLiteralCollection }
  | { readonly tag: "map"; readonly value: GenericLiteralMap }
  | { readonly tag: "keyword"; readonly value: Keyword }
  | { readonly tag: "identifier"; readonly value: Identifier };

export type IntegerLiteral = bigint & { readonly __brand: "IntegerLiteral" };

export type FloatLiteral = number & { readonly __brand: "FloatLiteral" };

export type NumericLiteral =
  | { readonly tag: "integer"; readonly value: IntegerLiteral }
  | { readonly tag: "float"; readonly value: FloatLiteral };

export type DateLiteral = StringArgument | null & { readonly __brand: "DateLiteral" };

export type Keyword =
  | { readonly tag: "edges" }
  | { readonly tag: "keys" }
  | { readonly tag: "new" }
  | { readonly tag: "values" };

export type Identifier = string & { readonly __brand: "Identifier" };
