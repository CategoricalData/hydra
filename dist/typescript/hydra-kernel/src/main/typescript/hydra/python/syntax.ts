// Note: this is an automatically generated file. Do not edit.

/**
 * A Python syntax model, based on the Python v3 PEG grammar retrieved on 2024-12-22 from https://docs.python.org/3/reference/grammar.html
 */



import * as Core from "../core.js";

export interface AnnotatedStatement {
  readonly comment: string;
  readonly statement: Statement;
}

export type Module = ReadonlyArray<ReadonlyArray<Statement>> & { readonly __brand: "Module" };

export type QuoteStyle =
  | { readonly tag: "single" }
  | { readonly tag: "double" }
  | { readonly tag: "triple" };

export type Name = string & { readonly __brand: "Name" };

export type Number =
  | { readonly tag: "integer"; readonly value: bigint }
  | { readonly tag: "float"; readonly value: number };

export interface String {
  readonly value: string;
  readonly quoteStyle: QuoteStyle;
}

export type TypeComment = string & { readonly __brand: "TypeComment" };

export type File = ReadonlyArray<Statement> & { readonly __brand: "File" };

export type Interactive = Statement & { readonly __brand: "Interactive" };

export type Eval = ReadonlyArray<Expression> & { readonly __brand: "Eval" };

export interface FuncType {
  readonly type: ReadonlyArray<TypeExpression>;
  readonly body: Expression;
}

export type Statement =
  | { readonly tag: "compound"; readonly value: CompoundStatement }
  | { readonly tag: "simple"; readonly value: ReadonlyArray<SimpleStatement> }
  | { readonly tag: "annotated"; readonly value: AnnotatedStatement };

export type SimpleStatement =
  | { readonly tag: "assignment"; readonly value: Assignment }
  | { readonly tag: "typeAlias"; readonly value: TypeAlias }
  | { readonly tag: "starExpressions"; readonly value: ReadonlyArray<StarExpression> }
  | { readonly tag: "return"; readonly value: ReturnStatement }
  | { readonly tag: "import"; readonly value: ImportStatement }
  | { readonly tag: "raise"; readonly value: RaiseStatement }
  | { readonly tag: "pass" }
  | { readonly tag: "del"; readonly value: DelStatement }
  | { readonly tag: "yield"; readonly value: YieldStatement }
  | { readonly tag: "assert"; readonly value: AssertStatement }
  | { readonly tag: "break" }
  | { readonly tag: "continue" }
  | { readonly tag: "global"; readonly value: ReadonlyArray<Name> }
  | { readonly tag: "nonlocal"; readonly value: ReadonlyArray<Name> };

export type CompoundStatement =
  | { readonly tag: "function"; readonly value: FunctionDefinition }
  | { readonly tag: "if"; readonly value: IfStatement }
  | { readonly tag: "classDef"; readonly value: ClassDefinition }
  | { readonly tag: "with"; readonly value: WithStatement }
  | { readonly tag: "for"; readonly value: ForStatement }
  | { readonly tag: "try"; readonly value: TryStatement }
  | { readonly tag: "while"; readonly value: WhileStatement }
  | { readonly tag: "match"; readonly value: MatchStatement };

export type Assignment =
  | { readonly tag: "typed"; readonly value: TypedAssignment }
  | { readonly tag: "untyped"; readonly value: UntypedAssignment }
  | { readonly tag: "aug"; readonly value: AugAssignment };

export interface TypedAssignment {
  readonly lhs: SingleTarget;
  readonly type: Expression;
  readonly rhs: AnnotatedRhs | null;
}

export interface UntypedAssignment {
  readonly targets: ReadonlyArray<StarTarget>;
  readonly rhs: AnnotatedRhs;
  readonly typeComment: TypeComment | null;
}

export interface AugAssignment {
  readonly lhs: SingleTarget;
  readonly augassign: AugAssign;
  readonly rhs: AnnotatedRhs;
}

export type AnnotatedRhs =
  | { readonly tag: "yield"; readonly value: YieldExpression }
  | { readonly tag: "star"; readonly value: ReadonlyArray<StarExpression> };

export type AugAssign =
  | { readonly tag: "plusEqual" }
  | { readonly tag: "minusEqual" }
  | { readonly tag: "timesEqual" }
  | { readonly tag: "atEqual" }
  | { readonly tag: "slashEqual" }
  | { readonly tag: "percentEqual" }
  | { readonly tag: "ampersandEqual" }
  | { readonly tag: "barEqual" }
  | { readonly tag: "caretEqual" }
  | { readonly tag: "leftShiftEqual" }
  | { readonly tag: "rightShiftEqual" }
  | { readonly tag: "starStarEqual" }
  | { readonly tag: "doubleSlashEqual" };

export type ReturnStatement = ReadonlyArray<StarExpression> & { readonly __brand: "ReturnStatement" };

export type RaiseStatement = RaiseExpression | null & { readonly __brand: "RaiseStatement" };

export interface RaiseExpression {
  readonly expression: Expression;
  readonly from: Expression | null;
}

export type DelStatement = DelTargets & { readonly __brand: "DelStatement" };

export type YieldStatement = YieldExpression & { readonly __brand: "YieldStatement" };

export interface AssertStatement {
  readonly expression1: Expression;
  readonly expression2: Expression | null;
}

export type ImportStatement =
  | { readonly tag: "name"; readonly value: ImportName }
  | { readonly tag: "from"; readonly value: ImportFrom };

export type ImportName = ReadonlyArray<DottedAsName> & { readonly __brand: "ImportName" };

export interface ImportFrom {
  readonly prefixes: ReadonlyArray<RelativeImportPrefix>;
  readonly dottedName: DottedName | null;
  readonly targets: ImportFromTargets;
}

export type RelativeImportPrefix =
  | { readonly tag: "dot" }
  | { readonly tag: "ellipsis" };

export type ImportFromTargets =
  | { readonly tag: "simple"; readonly value: ReadonlyArray<ImportFromAsName> }
  | { readonly tag: "parens"; readonly value: ReadonlyArray<ImportFromAsName> }
  | { readonly tag: "star" };

export interface ImportFromAsName {
  readonly name: Name;
  readonly as: Name | null;
}

export interface DottedAsName {
  readonly name: DottedName;
  readonly as: Name | null;
}

export type DottedName = ReadonlyArray<Name> & { readonly __brand: "DottedName" };

export type Block =
  | { readonly tag: "indented"; readonly value: ReadonlyArray<ReadonlyArray<Statement>> }
  | { readonly tag: "simple"; readonly value: ReadonlyArray<SimpleStatement> };

export type Decorators = ReadonlyArray<NamedExpression> & { readonly __brand: "Decorators" };

export interface ClassDefinition {
  readonly decorators: Decorators | null;
  readonly name: Name;
  readonly typeParams: ReadonlyArray<TypeParameter>;
  readonly arguments: Args | null;
  readonly body: Block;
}

export interface FunctionDefinition {
  readonly decorators: Decorators | null;
  readonly raw: FunctionDefRaw;
}

export interface FunctionDefRaw {
  readonly async: boolean;
  readonly name: Name;
  readonly typeParams: ReadonlyArray<TypeParameter>;
  readonly params: Parameters | null;
  readonly returnType: Expression | null;
  readonly funcTypeComment: FuncTypeComment | null;
  readonly block: Block;
}

export type Parameters =
  | { readonly tag: "slashNoDefault"; readonly value: SlashNoDefaultParameters }
  | { readonly tag: "slashWithDefault"; readonly value: SlashWithDefaultParameters }
  | { readonly tag: "paramNoDefault"; readonly value: ParamNoDefaultParameters }
  | { readonly tag: "paramWithDefault"; readonly value: ParamWithDefaultParameters }
  | { readonly tag: "starEtc"; readonly value: StarEtc };

export interface SlashNoDefaultParameters {
  readonly slash: SlashNoDefault;
  readonly paramNoDefault: ReadonlyArray<ParamNoDefault>;
  readonly paramWithDefault: ReadonlyArray<ParamWithDefault>;
  readonly starEtc: StarEtc | null;
}

export interface SlashWithDefaultParameters {
  readonly paramNoDefault: ReadonlyArray<ParamNoDefault>;
  readonly paramWithDefault: ReadonlyArray<ParamWithDefault>;
  readonly starEtc: StarEtc | null;
}

export interface ParamNoDefaultParameters {
  readonly paramNoDefault: ReadonlyArray<ParamNoDefault>;
  readonly paramWithDefault: ReadonlyArray<ParamWithDefault>;
  readonly starEtc: StarEtc | null;
}

export interface ParamWithDefaultParameters {
  readonly paramWithDefault: ReadonlyArray<ParamWithDefault>;
  readonly starEtc: StarEtc | null;
}

export type SlashNoDefault = ReadonlyArray<ParamNoDefault> & { readonly __brand: "SlashNoDefault" };

export interface SlashWithDefault {
  readonly paramNoDefault: ReadonlyArray<ParamNoDefault>;
  readonly paramWithDefault: ReadonlyArray<ParamWithDefault>;
}

export type StarEtc =
  | { readonly tag: "starNoDefault"; readonly value: NoDefaultStarEtc }
  | { readonly tag: "starNoDefaultStarAnnotation"; readonly value: NoDefaultStarAnnotationStarEtc }
  | { readonly tag: "starComma"; readonly value: CommaStarEtc }
  | { readonly tag: "keywords"; readonly value: Keywords };

export interface NoDefaultStarEtc {
  readonly paramNoDefault: ParamNoDefault;
  readonly paramMaybeDefault: ReadonlyArray<ParamMaybeDefault>;
  readonly keywords: Keywords | null;
}

export interface NoDefaultStarAnnotationStarEtc {
  readonly paramNoDefaultStarAnnotation: ParamNoDefaultStarAnnotation;
  readonly paramMaybeDefault: ReadonlyArray<ParamMaybeDefault>;
  readonly keywords: Keywords | null;
}

export interface CommaStarEtc {
  readonly paramMaybeDefault: ReadonlyArray<ParamMaybeDefault>;
  readonly keywords: Keywords | null;
}

export type Keywords = ParamNoDefault & { readonly __brand: "Keywords" };

export interface ParamNoDefault {
  readonly param: Param;
  readonly typeComment: TypeComment | null;
}

export interface ParamNoDefaultStarAnnotation {
  readonly paramStarAnnotation: ParamStarAnnotation;
  readonly typeComment: TypeComment | null;
}

export interface ParamWithDefault {
  readonly param: Param;
  readonly default: Default;
  readonly typeComment: TypeComment | null;
}

export interface ParamMaybeDefault {
  readonly param: Param;
  readonly default: Default | null;
  readonly typeComment: TypeComment | null;
}

export interface Param {
  readonly name: Name;
  readonly annotation: Annotation | null;
}

export interface ParamStarAnnotation {
  readonly name: Name;
  readonly annotation: StarAnnotation;
}

export type Annotation = Expression & { readonly __brand: "Annotation" };

export type StarAnnotation = StarExpression & { readonly __brand: "StarAnnotation" };

export type Default = Expression & { readonly __brand: "Default" };

export interface IfStatement {
  readonly condition: NamedExpression;
  readonly body: Block;
  readonly continuation: IfTail | null;
}

export type IfTail =
  | { readonly tag: "elif"; readonly value: IfStatement }
  | { readonly tag: "else"; readonly value: Block };

export interface WhileStatement {
  readonly condition: NamedExpression;
  readonly body: Block;
  readonly else: Block | null;
}

export interface ForStatement {
  readonly async: boolean;
  readonly targets: ReadonlyArray<StarTarget>;
  readonly expressions: ReadonlyArray<StarExpression>;
  readonly typeComment: TypeComment | null;
  readonly body: Block;
  readonly else: Block | null;
}

export interface WithStatement {
  readonly async: boolean;
  readonly items: ReadonlyArray<WithItem>;
  readonly typeComment: TypeComment | null;
  readonly body: Block;
}

export interface WithItem {
  readonly expression: Expression;
  readonly as: StarTarget | null;
}

export type TryStatement =
  | { readonly tag: "finally"; readonly value: TryFinallyStatement }
  | { readonly tag: "except"; readonly value: TryExceptStatement }
  | { readonly tag: "exceptStar"; readonly value: TryExceptStarStatement };

export interface TryFinallyStatement {
  readonly body: Block;
  readonly finally: Block;
}

export interface TryExceptStatement {
  readonly body: Block;
  readonly excepts: ReadonlyArray<ExceptBlock>;
  readonly else: Block | null;
  readonly finally: Block | null;
}

export interface TryExceptStarStatement {
  readonly body: Block;
  readonly excepts: ReadonlyArray<ExceptStarBlock>;
  readonly else: Block | null;
  readonly finally: Block | null;
}

export interface ExceptBlock {
  readonly expression: ExceptExpression | null;
  readonly body: Block;
}

export interface ExceptExpression {
  readonly expression: Expression;
  readonly as: Name | null;
}

export interface ExceptStarBlock {
  readonly expression: Expression;
  readonly as: Name | null;
  readonly body: Block;
}

export interface MatchStatement {
  readonly subject: SubjectExpression;
  readonly cases: ReadonlyArray<CaseBlock>;
}

export type SubjectExpression =
  | { readonly tag: "tuple"; readonly value: ReadonlyArray<StarNamedExpression> }
  | { readonly tag: "simple"; readonly value: NamedExpression };

export interface CaseBlock {
  readonly patterns: Patterns;
  readonly guard: Guard | null;
  readonly body: Block;
}

export type Guard = NamedExpression & { readonly __brand: "Guard" };

export type Patterns =
  | { readonly tag: "sequence"; readonly value: OpenSequencePattern }
  | { readonly tag: "pattern"; readonly value: Pattern };

export type Pattern =
  | { readonly tag: "as"; readonly value: AsPattern }
  | { readonly tag: "or"; readonly value: OrPattern };

export interface AsPattern {
  readonly pattern: OrPattern;
  readonly as: PatternCaptureTarget;
}

export type OrPattern = ReadonlyArray<ClosedPattern> & { readonly __brand: "OrPattern" };

export type ClosedPattern =
  | { readonly tag: "literal"; readonly value: LiteralExpression }
  | { readonly tag: "capture"; readonly value: CapturePattern }
  | { readonly tag: "wildcard" }
  | { readonly tag: "value"; readonly value: ValuePattern }
  | { readonly tag: "group"; readonly value: GroupPattern }
  | { readonly tag: "sequence"; readonly value: SequencePattern }
  | { readonly tag: "mapping"; readonly value: MappingPattern }
  | { readonly tag: "class"; readonly value: ClassPattern };

export type LiteralExpression =
  | { readonly tag: "number"; readonly value: SignedNumber }
  | { readonly tag: "complex"; readonly value: ComplexNumber }
  | { readonly tag: "string"; readonly value: string }
  | { readonly tag: "none" }
  | { readonly tag: "true" }
  | { readonly tag: "false" };

export interface ComplexNumber {
  readonly real: SignedRealNumber;
  readonly plusOrMinus: PlusOrMinus;
  readonly imaginary: ImaginaryNumber;
}

export type PlusOrMinus =
  | { readonly tag: "plus" }
  | { readonly tag: "minus" };

export type SignedNumber =
  | { readonly tag: "sign"; readonly value: PlusOrMinus }
  | { readonly tag: "number"; readonly value: Number };

export type SignedRealNumber =
  | { readonly tag: "sign"; readonly value: PlusOrMinus }
  | { readonly tag: "number"; readonly value: RealNumber };

export type RealNumber = Number & { readonly __brand: "RealNumber" };

export type ImaginaryNumber = Number & { readonly __brand: "ImaginaryNumber" };

export type CapturePattern = PatternCaptureTarget & { readonly __brand: "CapturePattern" };

export type PatternCaptureTarget = Name & { readonly __brand: "PatternCaptureTarget" };

export type ValuePattern = Attribute & { readonly __brand: "ValuePattern" };

export type Attribute = ReadonlyArray<Name> & { readonly __brand: "Attribute" };

export type NameOrAttribute = ReadonlyArray<Name> & { readonly __brand: "NameOrAttribute" };

export type GroupPattern = Pattern & { readonly __brand: "GroupPattern" };

export type SequencePattern =
  | { readonly tag: "list"; readonly value: MaybeSequencePattern | null }
  | { readonly tag: "tuple"; readonly value: OpenSequencePattern | null };

export interface OpenSequencePattern {
  readonly head: MaybeStarPattern;
  readonly tail: MaybeSequencePattern | null;
}

export type MaybeSequencePattern = ReadonlyArray<MaybeStarPattern> & { readonly __brand: "MaybeSequencePattern" };

export type MaybeStarPattern =
  | { readonly tag: "star"; readonly value: StarPattern }
  | { readonly tag: "pattern"; readonly value: Pattern };

export type StarPattern =
  | { readonly tag: "capture"; readonly value: PatternCaptureTarget }
  | { readonly tag: "wildcard" };

export interface MappingPattern {
  readonly items: ItemsPattern | null;
  readonly doubleStar: DoubleStarPattern | null;
}

export type ItemsPattern = ReadonlyArray<KeyValuePattern> & { readonly __brand: "ItemsPattern" };

export interface KeyValuePattern {
  readonly key: LiteralExpressionOrAttribute;
  readonly value: Pattern;
}

export type LiteralExpressionOrAttribute =
  | { readonly tag: "literal"; readonly value: LiteralExpression }
  | { readonly tag: "attribute"; readonly value: Attribute };

export type DoubleStarPattern = PatternCaptureTarget & { readonly __brand: "DoubleStarPattern" };

export interface ClassPattern {
  readonly nameOrAttribute: NameOrAttribute;
  readonly positionalPatterns: PositionalPatterns | null;
  readonly keywordPatterns: KeywordPatterns | null;
}

export type PositionalPatterns = ReadonlyArray<Pattern> & { readonly __brand: "PositionalPatterns" };

export type KeywordPatterns = ReadonlyArray<KeywordPattern> & { readonly __brand: "KeywordPatterns" };

export interface KeywordPattern {
  readonly name: Name;
  readonly pattern: Pattern;
}

export interface TypeAlias {
  readonly name: Name;
  readonly typeParams: ReadonlyArray<TypeParameter>;
  readonly expression: Expression;
}

export type TypeParameter =
  | { readonly tag: "simple"; readonly value: SimpleTypeParameter }
  | { readonly tag: "star"; readonly value: StarTypeParameter }
  | { readonly tag: "doubleStar"; readonly value: DoubleStarTypeParameter };

export interface SimpleTypeParameter {
  readonly name: Name;
  readonly bound: Expression | null;
  readonly default: Expression | null;
}

export interface StarTypeParameter {
  readonly name: Name;
  readonly default: StarExpression | null;
}

export interface DoubleStarTypeParameter {
  readonly name: Name;
  readonly default: Expression | null;
}

export type Expression =
  | { readonly tag: "conditional"; readonly value: Conditional }
  | { readonly tag: "simple"; readonly value: Disjunction }
  | { readonly tag: "lambda"; readonly value: Lambda };

export interface Conditional {
  readonly body: Disjunction;
  readonly if: Disjunction;
  readonly else: Expression;
}

export type YieldExpression =
  | { readonly tag: "from"; readonly value: Expression }
  | { readonly tag: "simple"; readonly value: ReadonlyArray<StarExpression> };

export type StarExpression =
  | { readonly tag: "star"; readonly value: BitwiseOr }
  | { readonly tag: "simple"; readonly value: Expression };

export type StarNamedExpressions = ReadonlyArray<StarNamedExpression> & { readonly __brand: "StarNamedExpressions" };

export type StarNamedExpression =
  | { readonly tag: "star"; readonly value: BitwiseOr }
  | { readonly tag: "simple"; readonly value: NamedExpression };

export interface AssignmentExpression {
  readonly name: Name;
  readonly expression: Expression;
}

export type NamedExpression =
  | { readonly tag: "assignment"; readonly value: AssignmentExpression }
  | { readonly tag: "simple"; readonly value: Expression };

export type Disjunction = ReadonlyArray<Conjunction> & { readonly __brand: "Disjunction" };

export type Conjunction = ReadonlyArray<Inversion> & { readonly __brand: "Conjunction" };

export type Inversion =
  | { readonly tag: "not"; readonly value: Inversion }
  | { readonly tag: "simple"; readonly value: Comparison };

export interface Comparison {
  readonly lhs: BitwiseOr;
  readonly rhs: ReadonlyArray<CompareOpBitwiseOrPair>;
}

export interface CompareOpBitwiseOrPair {
  readonly operator: CompareOp;
  readonly rhs: BitwiseOr;
}

export type CompareOp =
  | { readonly tag: "eq" }
  | { readonly tag: "noteq" }
  | { readonly tag: "lte" }
  | { readonly tag: "lt" }
  | { readonly tag: "gte" }
  | { readonly tag: "gt" }
  | { readonly tag: "notin" }
  | { readonly tag: "in" }
  | { readonly tag: "isnot" }
  | { readonly tag: "is" };

export interface BitwiseOr {
  readonly lhs: BitwiseOr | null;
  readonly rhs: BitwiseXor;
}

export interface BitwiseXor {
  readonly lhs: BitwiseXor | null;
  readonly rhs: BitwiseAnd;
}

export interface BitwiseAnd {
  readonly lhs: BitwiseAnd | null;
  readonly rhs: ShiftExpression;
}

export interface ShiftExpression {
  readonly lhs: ShiftLhs | null;
  readonly rhs: Sum;
}

export interface ShiftLhs {
  readonly operand: ShiftExpression;
  readonly operator: ShiftOp;
}

export type ShiftOp =
  | { readonly tag: "left" }
  | { readonly tag: "right" };

export interface Sum {
  readonly lhs: SumLhs | null;
  readonly rhs: Term;
}

export interface SumLhs {
  readonly operand: Sum;
  readonly operator: SumOp;
}

export type SumOp =
  | { readonly tag: "add" }
  | { readonly tag: "sub" };

export interface Term {
  readonly lhs: TermLhs | null;
  readonly rhs: Factor;
}

export interface TermLhs {
  readonly operand: Term;
  readonly operator: TermOp;
}

export type TermOp =
  | { readonly tag: "mul" }
  | { readonly tag: "div" }
  | { readonly tag: "floordiv" }
  | { readonly tag: "mod" }
  | { readonly tag: "matmul" };

export type Factor =
  | { readonly tag: "positive"; readonly value: Factor }
  | { readonly tag: "negative"; readonly value: Factor }
  | { readonly tag: "complement"; readonly value: Factor }
  | { readonly tag: "simple"; readonly value: Power };

export interface Power {
  readonly lhs: AwaitPrimary;
  readonly rhs: Factor | null;
}

export interface AwaitPrimary {
  readonly await: boolean;
  readonly primary: Primary;
}

export type Primary =
  | { readonly tag: "simple"; readonly value: Atom }
  | { readonly tag: "compound"; readonly value: PrimaryWithRhs };

export interface PrimaryWithRhs {
  readonly primary: Primary;
  readonly rhs: PrimaryRhs;
}

export type PrimaryRhs =
  | { readonly tag: "project"; readonly value: Name }
  | { readonly tag: "genexp"; readonly value: Genexp }
  | { readonly tag: "call"; readonly value: Args }
  | { readonly tag: "slices"; readonly value: Slices };

export interface Slices {
  readonly head: Slice;
  readonly tail: ReadonlyArray<SliceOrStarredExpression>;
}

export type SliceOrStarredExpression =
  | { readonly tag: "slice"; readonly value: Slice }
  | { readonly tag: "starred"; readonly value: StarredExpression };

export type Slice =
  | { readonly tag: "named"; readonly value: NamedExpression }
  | { readonly tag: "slice_"; readonly value: SliceExpression };

export interface SliceExpression {
  readonly start: Expression | null;
  readonly stop: Expression | null;
  readonly step: Expression | null;
}

export type Atom =
  | { readonly tag: "name"; readonly value: Name }
  | { readonly tag: "true" }
  | { readonly tag: "false" }
  | { readonly tag: "none" }
  | { readonly tag: "string"; readonly value: String }
  | { readonly tag: "number"; readonly value: Number }
  | { readonly tag: "tuple"; readonly value: Tuple }
  | { readonly tag: "group"; readonly value: Group }
  | { readonly tag: "genexp"; readonly value: Genexp }
  | { readonly tag: "list"; readonly value: List }
  | { readonly tag: "listcomp"; readonly value: Listcomp }
  | { readonly tag: "dict"; readonly value: Dict }
  | { readonly tag: "set"; readonly value: Set }
  | { readonly tag: "dictcomp"; readonly value: Dictcomp }
  | { readonly tag: "setcomp"; readonly value: Setcomp }
  | { readonly tag: "ellipsis" };

export type Group =
  | { readonly tag: "yield"; readonly value: YieldExpression }
  | { readonly tag: "expression"; readonly value: NamedExpression };

export interface Lambda {
  readonly params: LambdaParameters;
  readonly body: Expression;
}

export interface LambdaParameters {
  readonly slashNoDefault: LambdaSlashNoDefault | null;
  readonly paramNoDefault: ReadonlyArray<LambdaParamNoDefault>;
  readonly paramWithDefault: ReadonlyArray<LambdaParamWithDefault>;
  readonly starEtc: LambdaStarEtc | null;
}

export interface LambdaSlashNoDefault {
  readonly parameters: ReadonlyArray<LambdaParamNoDefault>;
}

export interface LambdaSlashWithDefault {
  readonly paramNoDefault: ReadonlyArray<LambdaParamNoDefault>;
  readonly paramWithDefault: ReadonlyArray<LambdaParamWithDefault>;
}

export type LambdaStarEtc =
  | { readonly tag: "star"; readonly value: boolean }
  | { readonly tag: "paramNoDefault"; readonly value: LambdaParamNoDefault }
  | { readonly tag: "paramMaybeDefault"; readonly value: ReadonlyArray<LambdaParamMaybeDefault> }
  | { readonly tag: "kwds"; readonly value: LambdaKwds };

export type LambdaKwds = LambdaParamNoDefault & { readonly __brand: "LambdaKwds" };

export type LambdaParamNoDefault = Name & { readonly __brand: "LambdaParamNoDefault" };

export interface LambdaParamWithDefault {
  readonly param: Name;
  readonly default: Default | null;
}

export interface LambdaParamMaybeDefault {
  readonly param: Name;
  readonly default: Default | null;
}

export type List = ReadonlyArray<StarNamedExpression> & { readonly __brand: "List" };

export type Tuple = ReadonlyArray<StarNamedExpression> & { readonly __brand: "Tuple" };

export type Set = ReadonlyArray<StarNamedExpression> & { readonly __brand: "Set" };

export type Dict = ReadonlyArray<DoubleStarredKvpair> & { readonly __brand: "Dict" };

export type DoubleStarredKvpair =
  | { readonly tag: "starred"; readonly value: BitwiseOr }
  | { readonly tag: "pair"; readonly value: Kvpair };

export interface Kvpair {
  readonly key: Expression;
  readonly value: Expression;
}

export type ForIfClauses = ReadonlyArray<ForIfClause> & { readonly __brand: "ForIfClauses" };

export interface ForIfClause {
  readonly async: boolean;
  readonly targets: ReadonlyArray<StarTarget>;
  readonly in: Disjunction;
  readonly ifs: ReadonlyArray<Disjunction>;
}

export interface Listcomp {
  readonly expression: NamedExpression;
  readonly forIfClauses: ForIfClauses;
}

export interface Setcomp {
  readonly expression: NamedExpression;
  readonly forIfClauses: ForIfClauses;
}

export interface Genexp {
  readonly head: GenexpHead;
  readonly tail: ForIfClauses;
}

export type GenexpHead =
  | { readonly tag: "assignment"; readonly value: AssignmentExpression }
  | { readonly tag: "expression"; readonly value: Expression };

export interface Dictcomp {
  readonly kvpair: Kvpair;
  readonly forIfClauses: ForIfClauses;
}

export interface Args {
  readonly positional: ReadonlyArray<PosArg>;
  readonly kwargOrStarred: ReadonlyArray<KwargOrStarred>;
  readonly kwargOrDoubleStarred: ReadonlyArray<KwargOrDoubleStarred>;
}

export type PosArg =
  | { readonly tag: "starred"; readonly value: StarredExpression }
  | { readonly tag: "assignment"; readonly value: AssignmentExpression }
  | { readonly tag: "expression"; readonly value: Expression };

export type StarredExpression = Expression & { readonly __brand: "StarredExpression" };

export type KwargOrStarred =
  | { readonly tag: "kwarg"; readonly value: Kwarg }
  | { readonly tag: "starred"; readonly value: StarredExpression };

export interface Kwarg {
  readonly name: Name;
  readonly value: Expression;
}

export type KwargOrDoubleStarred =
  | { readonly tag: "kwarg"; readonly value: Kwarg }
  | { readonly tag: "doubleStarred"; readonly value: Expression };

export type StarTargetsListSeq = ReadonlyArray<StarTarget> & { readonly __brand: "StarTargetsListSeq" };

export type StarTargetsTupleSeq = ReadonlyArray<StarTarget> & { readonly __brand: "StarTargetsTupleSeq" };

export type StarTarget =
  | { readonly tag: "starred"; readonly value: StarTarget }
  | { readonly tag: "unstarred"; readonly value: TargetWithStarAtom };

export type TargetWithStarAtom =
  | { readonly tag: "project"; readonly value: TPrimaryAndName }
  | { readonly tag: "slices"; readonly value: TPrimaryAndSlices }
  | { readonly tag: "atom"; readonly value: StarAtom };

export interface TPrimaryAndName {
  readonly primary: TPrimary;
  readonly name: Name;
}

export interface TPrimaryAndSlices {
  readonly primary: TPrimary;
  readonly slices: Slices;
}

export type StarAtom =
  | { readonly tag: "name"; readonly value: Name }
  | { readonly tag: "targetWithStarAtom"; readonly value: TargetWithStarAtom }
  | { readonly tag: "starTargetsTupleSeq"; readonly value: StarTargetsTupleSeq | null }
  | { readonly tag: "starTargetsListSeq"; readonly value: StarTargetsListSeq | null };

export type SingleTarget =
  | { readonly tag: "subscriptAttributeTarget"; readonly value: SingleSubscriptAttributeTarget }
  | { readonly tag: "name"; readonly value: Name }
  | { readonly tag: "parens"; readonly value: SingleTarget };

export type SingleSubscriptAttributeTarget =
  | { readonly tag: "primaryAndName"; readonly value: TPrimaryAndName }
  | { readonly tag: "primaryAndSlices"; readonly value: TPrimaryAndSlices };

export type TPrimary =
  | { readonly tag: "primaryAndName"; readonly value: TPrimaryAndName }
  | { readonly tag: "primaryAndSlices"; readonly value: TPrimaryAndSlices }
  | { readonly tag: "primaryAndGenexp"; readonly value: TPrimaryAndGenexp }
  | { readonly tag: "primaryAndArguments"; readonly value: TPrimaryAndArguments }
  | { readonly tag: "atom"; readonly value: Atom };

export interface TPrimaryAndGenexp {
  readonly primary: TPrimary;
  readonly genexp: Genexp;
}

export interface TPrimaryAndArguments {
  readonly primary: TPrimary;
  readonly arguments: Args | null;
}

export type DelTargets = ReadonlyArray<DelTarget> & { readonly __brand: "DelTargets" };

export type DelTarget =
  | { readonly tag: "primaryAndName"; readonly value: TPrimaryAndName }
  | { readonly tag: "primaryAndSlices"; readonly value: TPrimaryAndSlices }
  | { readonly tag: "delTAtom"; readonly value: DelTAtom };

export type DelTAtom =
  | { readonly tag: "name"; readonly value: Name }
  | { readonly tag: "target"; readonly value: DelTarget }
  | { readonly tag: "targets"; readonly value: DelTargets };

export type TypeExpression =
  | { readonly tag: "expression"; readonly value: Expression }
  | { readonly tag: "starredExpression"; readonly value: Expression }
  | { readonly tag: "doubleStarredExpression"; readonly value: Expression };

export type FuncTypeComment = TypeComment & { readonly __brand: "FuncTypeComment" };
