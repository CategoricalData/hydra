// Note: this is an automatically generated file. Do not edit.

/**
 * A Cypher model based on the OpenCypher specification (version 23), copyright Neo Technology, available at:
 *   https://opencypher.org/resources/
 */



import * as Core from "../core.js";

export type Query =
  | { readonly tag: "regular"; readonly value: RegularQuery }
  | { readonly tag: "standalone"; readonly value: StandaloneCall };

export interface RegularQuery {
  readonly head: SingleQuery;
  readonly rest: ReadonlyArray<Union>;
}

export interface Union {
  readonly all: boolean;
  readonly query: SingleQuery;
}

export type SingleQuery =
  | { readonly tag: "singlePart"; readonly value: SinglePartQuery }
  | { readonly tag: "multiPart"; readonly value: MultiPartQuery };

export interface SinglePartQuery {
  readonly reading: ReadonlyArray<ReadingClause>;
  readonly updating: ReadonlyArray<UpdatingClause>;
  readonly return: Return | null;
}

export interface WithClause {
  readonly reading: ReadonlyArray<ReadingClause>;
  readonly updating: ReadonlyArray<UpdatingClause>;
  readonly with: With;
}

export interface MultiPartQuery {
  readonly with: ReadonlyArray<WithClause>;
  readonly body: SinglePartQuery;
}

export type UpdatingClause =
  | { readonly tag: "create"; readonly value: Create }
  | { readonly tag: "merge"; readonly value: Merge }
  | { readonly tag: "delete"; readonly value: Delete }
  | { readonly tag: "set"; readonly value: Set }
  | { readonly tag: "remove"; readonly value: Remove };

export type ReadingClause =
  | { readonly tag: "match"; readonly value: Match }
  | { readonly tag: "unwind"; readonly value: Unwind }
  | { readonly tag: "inQueryCall"; readonly value: InQueryCall };

export interface Match {
  readonly optional: boolean;
  readonly pattern: Pattern;
  readonly where: Where | null;
}

export interface Unwind {
  readonly expression: Expression;
  readonly variable: Variable;
}

export interface Merge {
  readonly patternPart: PatternPart;
  readonly actions: ReadonlyArray<MergeAction>;
}

export type MatchOrCreate =
  | { readonly tag: "match" }
  | { readonly tag: "create" };

export interface MergeAction {
  readonly action: MatchOrCreate;
  readonly set: Set;
}

export type Create = Pattern & { readonly __brand: "Create" };

export type Set = ReadonlyArray<SetItem> & { readonly __brand: "Set" };

export type SetItem =
  | { readonly tag: "property"; readonly value: PropertyEquals }
  | { readonly tag: "variableEqual"; readonly value: VariableEquals }
  | { readonly tag: "variablePlusEqual"; readonly value: VariablePlusEquals }
  | { readonly tag: "variableLabels"; readonly value: VariableAndNodeLabels };

export interface PropertyEquals {
  readonly lhs: PropertyExpression;
  readonly rhs: Expression;
}

export interface VariableEquals {
  readonly lhs: Variable;
  readonly rhs: Expression;
}

export interface VariablePlusEquals {
  readonly lhs: Variable;
  readonly rhs: Expression;
}

export interface VariableAndNodeLabels {
  readonly variable: Variable;
  readonly labels: NodeLabels;
}

export interface Delete {
  readonly detach: boolean;
  readonly expressions: ReadonlyArray<Expression>;
}

export type Remove = ReadonlyArray<RemoveItem> & { readonly __brand: "Remove" };

export type RemoveItem =
  | { readonly tag: "variableLabels"; readonly value: VariableAndNodeLabels }
  | { readonly tag: "property"; readonly value: PropertyExpression };

export interface InQueryCall {
  readonly call: ExplicitProcedureInvocation;
  readonly yieldItems: YieldItems | null;
}

export type ProcedureInvocation =
  | { readonly tag: "explicit"; readonly value: ExplicitProcedureInvocation }
  | { readonly tag: "implicit"; readonly value: ImplicitProcedureInvocation };

export type StarOrYieldItems =
  | { readonly tag: "star" }
  | { readonly tag: "items"; readonly value: YieldItems };

export interface StandaloneCall {
  readonly call: ProcedureInvocation;
  readonly yieldItems: StarOrYieldItems | null;
}

export interface YieldItems {
  readonly items: ReadonlyArray<YieldItem>;
  readonly where: Where | null;
}

export interface YieldItem {
  readonly field: ProcedureResultField | null;
  readonly variable: Variable;
}

export interface With {
  readonly projection: ProjectionBody;
  readonly where: Where | null;
}

export type Return = ProjectionBody & { readonly __brand: "Return" };

export interface ProjectionBody {
  readonly distinct: boolean;
  readonly projectionItems: ProjectionItems;
  readonly order: Order | null;
  readonly skip: Skip | null;
  readonly limit: Limit | null;
}

export interface ProjectionItems {
  readonly star: boolean;
  readonly explicit: ReadonlyArray<ProjectionItem>;
}

export interface ProjectionItem {
  readonly expression: Expression;
  readonly variable: Variable | null;
}

export type Order = ReadonlyArray<SortItem> & { readonly __brand: "Order" };

export type Skip = Expression & { readonly __brand: "Skip" };

export type Limit = Expression & { readonly __brand: "Limit" };

export type SortOrder =
  | { readonly tag: "ascending" }
  | { readonly tag: "descending" };

export interface SortItem {
  readonly expression: Expression;
  readonly order: SortOrder | null;
}

export type Where = Expression & { readonly __brand: "Where" };

export type Pattern = ReadonlyArray<PatternPart> & { readonly __brand: "Pattern" };

export interface PatternPart {
  readonly variable: Variable | null;
  readonly pattern: AnonymousPatternPart;
}

export type AnonymousPatternPart = PatternElement & { readonly __brand: "AnonymousPatternPart" };

export interface NodePatternChain {
  readonly nodePattern: NodePattern;
  readonly chain: ReadonlyArray<PatternElementChain>;
}

export type PatternElement =
  | { readonly tag: "chained"; readonly value: NodePatternChain }
  | { readonly tag: "parenthesized"; readonly value: PatternElement };

export interface RelationshipsPattern {
  readonly nodePattern: NodePattern;
  readonly chain: ReadonlyArray<PatternElementChain>;
}

export interface NodePattern {
  readonly variable: Variable | null;
  readonly labels: NodeLabels | null;
  readonly properties: Properties | null;
}

export interface PatternElementChain {
  readonly relationship: RelationshipPattern;
  readonly node: NodePattern;
}

export interface RelationshipPattern {
  readonly leftArrow: boolean;
  readonly detail: RelationshipDetail | null;
  readonly rightArrow: boolean;
}

export interface RelationshipDetail {
  readonly variable: Variable | null;
  readonly types: RelationshipTypes | null;
  readonly range: RangeLiteral | null;
  readonly properties: Properties | null;
}

export type Properties =
  | { readonly tag: "map"; readonly value: MapLiteral }
  | { readonly tag: "parameter"; readonly value: Parameter };

export type RelationshipTypes = ReadonlyArray<RelTypeName> & { readonly __brand: "RelationshipTypes" };

export type NodeLabels = ReadonlyArray<NodeLabel> & { readonly __brand: "NodeLabels" };

export type NodeLabel = string & { readonly __brand: "NodeLabel" };

export interface RangeLiteral {
  readonly start: bigint | null;
  readonly end: bigint | null;
}

export type RelTypeName = string & { readonly __brand: "RelTypeName" };

export interface PropertyExpression {
  readonly atom: Atom;
  readonly lookups: ReadonlyArray<PropertyLookup>;
}

export type Expression = OrExpression & { readonly __brand: "Expression" };

export type OrExpression = ReadonlyArray<XorExpression> & { readonly __brand: "OrExpression" };

export type XorExpression = ReadonlyArray<AndExpression> & { readonly __brand: "XorExpression" };

export type AndExpression = ReadonlyArray<NotExpression> & { readonly __brand: "AndExpression" };

export interface NotExpression {
  readonly not: boolean;
  readonly expression: ComparisonExpression;
}

export interface ComparisonExpression {
  readonly left: StringListNullPredicateExpression;
  readonly right: ReadonlyArray<PartialComparisonExpression>;
}

export type ComparisonOperator =
  | { readonly tag: "eq" }
  | { readonly tag: "neq" }
  | { readonly tag: "lt" }
  | { readonly tag: "gt" }
  | { readonly tag: "lte" }
  | { readonly tag: "gte" };

export interface PartialComparisonExpression {
  readonly operator: ComparisonOperator;
  readonly right: StringListNullPredicateExpression;
}

export interface StringListNullPredicateExpression {
  readonly left: AddOrSubtractExpression;
  readonly right: ReadonlyArray<StringListNullPredicateRightHandSide>;
}

export type StringListNullPredicateRightHandSide =
  | { readonly tag: "string"; readonly value: StringPredicateExpression }
  | { readonly tag: "list"; readonly value: ListPredicateExpression }
  | { readonly tag: "null"; readonly value: NullPredicateExpression };

export interface StringPredicateExpression {
  readonly operator: StringPredicateOperator;
  readonly expression: AddOrSubtractExpression;
}

export type StringPredicateOperator =
  | { readonly tag: "startsWith" }
  | { readonly tag: "endsWith" }
  | { readonly tag: "contains" };

export type ListPredicateExpression = AddOrSubtractExpression & { readonly __brand: "ListPredicateExpression" };

export type NullPredicateExpression = boolean & { readonly __brand: "NullPredicateExpression" };

export interface AddOrSubtractExpression {
  readonly left: MultiplyDivideModuloExpression;
  readonly right: ReadonlyArray<AddOrSubtractRightHandSide>;
}

export interface AddOrSubtractRightHandSide {
  readonly operator: AddOrSubtractOperator;
  readonly expression: MultiplyDivideModuloExpression;
}

export type AddOrSubtractOperator =
  | { readonly tag: "add" }
  | { readonly tag: "subtract" };

export interface MultiplyDivideModuloExpression {
  readonly left: PowerOfExpression;
  readonly right: ReadonlyArray<MultiplyDivideModuloRightHandSide>;
}

export interface MultiplyDivideModuloRightHandSide {
  readonly operator: MultiplyDivideModuloOperator;
  readonly expression: PowerOfExpression;
}

export type MultiplyDivideModuloOperator =
  | { readonly tag: "multiply" }
  | { readonly tag: "divide" }
  | { readonly tag: "modulo" };

export type PowerOfExpression = ReadonlyArray<UnaryAddOrSubtractExpression> & { readonly __brand: "PowerOfExpression" };

export interface UnaryAddOrSubtractExpression {
  readonly operator: AddOrSubtractOperator | null;
  readonly expression: NonArithmeticOperatorExpression;
}

export type ListOperatorExpressionOrPropertyLookup =
  | { readonly tag: "list"; readonly value: ListOperatorExpression }
  | { readonly tag: "property"; readonly value: PropertyLookup };

export interface NonArithmeticOperatorExpression {
  readonly atom: Atom;
  readonly listsAndLookups: ReadonlyArray<ListOperatorExpressionOrPropertyLookup>;
  readonly labels: NodeLabels | null;
}

export interface RangeExpression {
  readonly start: Expression | null;
  readonly end: Expression | null;
}

export type ListOperatorExpression =
  | { readonly tag: "single"; readonly value: Expression }
  | { readonly tag: "range"; readonly value: RangeExpression };

export type PropertyLookup = PropertyKeyName & { readonly __brand: "PropertyLookup" };

export type Atom =
  | { readonly tag: "literal"; readonly value: Literal }
  | { readonly tag: "parameter"; readonly value: Parameter }
  | { readonly tag: "case"; readonly value: CaseExpression }
  | { readonly tag: "countStar" }
  | { readonly tag: "listComprehension"; readonly value: ListComprehension }
  | { readonly tag: "patternComprehension"; readonly value: PatternComprehension }
  | { readonly tag: "quantifier"; readonly value: Quantifier }
  | { readonly tag: "patternPredicate"; readonly value: PatternPredicate }
  | { readonly tag: "parenthesized"; readonly value: ParenthesizedExpression }
  | { readonly tag: "functionInvocation"; readonly value: FunctionInvocation }
  | { readonly tag: "existentialSubquery"; readonly value: ExistentialSubquery }
  | { readonly tag: "variable"; readonly value: Variable };

export interface CaseExpression {
  readonly expression: Expression | null;
  readonly alternatives: ReadonlyArray<CaseAlternative>;
  readonly else: Expression | null;
}

export interface CaseAlternative {
  readonly condition: Expression;
  readonly result: Expression;
}

export interface ListComprehension {
  readonly left: FilterExpression;
  readonly right: Expression | null;
}

export interface PatternComprehension {
  readonly variable: Variable | null;
  readonly pattern: RelationshipsPattern;
  readonly where: Where | null;
  readonly right: Expression;
}

export interface Quantifier {
  readonly operator: QuantifierOperator;
  readonly expression: FilterExpression;
}

export type QuantifierOperator =
  | { readonly tag: "all" }
  | { readonly tag: "any" }
  | { readonly tag: "none" }
  | { readonly tag: "single" };

export interface FilterExpression {
  readonly idInColl: IdInColl;
  readonly where: Where | null;
}

export type PatternPredicate = RelationshipsPattern & { readonly __brand: "PatternPredicate" };

export type ParenthesizedExpression = Expression & { readonly __brand: "ParenthesizedExpression" };

export interface IdInColl {
  readonly variable: Variable;
  readonly expression: Expression;
}

export interface FunctionInvocation {
  readonly name: QualifiedName;
  readonly distinct: boolean;
  readonly arguments: ReadonlyArray<Expression>;
}

export interface QualifiedName {
  readonly namespace: string;
  readonly local: string;
}

export interface PatternWhere {
  readonly pattern: Pattern;
  readonly where: Where | null;
}

export type ExistentialSubquery =
  | { readonly tag: "regular"; readonly value: RegularQuery }
  | { readonly tag: "pattern"; readonly value: PatternWhere };

export interface ExplicitProcedureInvocation {
  readonly name: QualifiedName;
  readonly arguments: ReadonlyArray<Expression>;
}

export type ImplicitProcedureInvocation = QualifiedName & { readonly __brand: "ImplicitProcedureInvocation" };

export type ProcedureResultField = string & { readonly __brand: "ProcedureResultField" };

export type Variable = string & { readonly __brand: "Variable" };

export type Literal =
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "null" }
  | { readonly tag: "number"; readonly value: NumberLiteral }
  | { readonly tag: "string"; readonly value: StringLiteral }
  | { readonly tag: "list"; readonly value: ListLiteral }
  | { readonly tag: "map"; readonly value: MapLiteral };

export type NumberLiteral =
  | { readonly tag: "double"; readonly value: number }
  | { readonly tag: "integer"; readonly value: bigint };

export type StringLiteral = string & { readonly __brand: "StringLiteral" };

export type ListLiteral = ReadonlyArray<Expression> & { readonly __brand: "ListLiteral" };

export type MapLiteral = ReadonlyArray<KeyValuePair> & { readonly __brand: "MapLiteral" };

export interface KeyValuePair {
  readonly key: PropertyKeyName;
  readonly value: Expression;
}

export type PropertyKeyName = string & { readonly __brand: "PropertyKeyName" };

export type Parameter =
  | { readonly tag: "symbolic"; readonly value: string }
  | { readonly tag: "integer"; readonly value: bigint };
