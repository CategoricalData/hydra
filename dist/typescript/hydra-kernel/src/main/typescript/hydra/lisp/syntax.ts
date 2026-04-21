// Note: this is an automatically generated file. Do not edit.

/**
 * A unified Lisp syntax model covering Clojure, Emacs Lisp, Common Lisp, and Scheme (R7RS). Designed for code generation from Hydra types and terms.
 */



import * as Core from "../core.js";

export interface Program {
  readonly dialect: Dialect;
  readonly module: ModuleDeclaration | null;
  readonly imports: ReadonlyArray<ImportDeclaration>;
  readonly exports: ReadonlyArray<ExportDeclaration>;
  readonly forms: ReadonlyArray<TopLevelFormWithComments>;
}

export type TopLevelForm =
  | { readonly tag: "function"; readonly value: FunctionDefinition }
  | { readonly tag: "variable"; readonly value: VariableDefinition }
  | { readonly tag: "constant"; readonly value: ConstantDefinition }
  | { readonly tag: "recordType"; readonly value: RecordTypeDefinition }
  | { readonly tag: "macro"; readonly value: MacroDefinition }
  | { readonly tag: "expression"; readonly value: Expression };

export interface TopLevelFormWithComments {
  readonly doc: Docstring | null;
  readonly comment: Comment | null;
  readonly form: TopLevelForm;
}

export interface FunctionDefinition {
  readonly name: Symbol;
  readonly params: ReadonlyArray<Symbol>;
  readonly restParam: Symbol | null;
  readonly doc: Docstring | null;
  readonly typeHints: ReadonlyArray<TypeHint>;
  readonly body: ReadonlyArray<Expression>;
}

export interface VariableDefinition {
  readonly name: Symbol;
  readonly value: Expression;
  readonly doc: Docstring | null;
}

export interface ConstantDefinition {
  readonly name: Symbol;
  readonly value: Expression;
  readonly doc: Docstring | null;
}

export interface RecordTypeDefinition {
  readonly name: Symbol;
  readonly fields: ReadonlyArray<FieldDefinition>;
  readonly doc: Docstring | null;
}

export interface FieldDefinition {
  readonly name: Symbol;
  readonly defaultValue: Expression | null;
}

export interface MacroDefinition {
  readonly name: Symbol;
  readonly params: ReadonlyArray<Symbol>;
  readonly restParam: Symbol | null;
  readonly body: ReadonlyArray<Expression>;
}

export type Expression =
  | { readonly tag: "application"; readonly value: Application }
  | { readonly tag: "lambda"; readonly value: Lambda }
  | { readonly tag: "let"; readonly value: LetExpression }
  | { readonly tag: "if"; readonly value: IfExpression }
  | { readonly tag: "cond"; readonly value: CondExpression }
  | { readonly tag: "case"; readonly value: CaseExpression }
  | { readonly tag: "and"; readonly value: AndExpression }
  | { readonly tag: "or"; readonly value: OrExpression }
  | { readonly tag: "not"; readonly value: NotExpression }
  | { readonly tag: "do"; readonly value: DoExpression }
  | { readonly tag: "begin"; readonly value: BeginExpression }
  | { readonly tag: "variable"; readonly value: VariableReference }
  | { readonly tag: "literal"; readonly value: Literal }
  | { readonly tag: "list"; readonly value: ListLiteral }
  | { readonly tag: "vector"; readonly value: VectorLiteral }
  | { readonly tag: "map"; readonly value: MapLiteral }
  | { readonly tag: "set"; readonly value: SetLiteral }
  | { readonly tag: "cons"; readonly value: ConsExpression }
  | { readonly tag: "dottedPair"; readonly value: DottedPair }
  | { readonly tag: "fieldAccess"; readonly value: FieldAccess }
  | { readonly tag: "typeAnnotation"; readonly value: TypeAnnotation }
  | { readonly tag: "quote"; readonly value: QuoteExpression }
  | { readonly tag: "quasiquote"; readonly value: QuasiquoteExpression }
  | { readonly tag: "unquote"; readonly value: UnquoteExpression }
  | { readonly tag: "splicingUnquote"; readonly value: SplicingUnquoteExpression }
  | { readonly tag: "sExpression"; readonly value: SExpression };

export interface Application {
  readonly function: Expression;
  readonly arguments: ReadonlyArray<Expression>;
}

export interface Lambda {
  readonly name: Symbol | null;
  readonly params: ReadonlyArray<Symbol>;
  readonly restParam: Symbol | null;
  readonly body: ReadonlyArray<Expression>;
}

export interface VariableReference {
  readonly name: Symbol;
  readonly functionNamespace: boolean;
}

export interface FieldAccess {
  readonly recordType: Symbol;
  readonly field: Symbol;
  readonly target: Expression;
}

export interface TypeAnnotation {
  readonly expression: Expression;
  readonly type: TypeSpecifier;
}

export interface IfExpression {
  readonly condition: Expression;
  readonly then: Expression;
  readonly else: Expression | null;
}

export interface CondExpression {
  readonly clauses: ReadonlyArray<CondClause>;
  readonly default: Expression | null;
}

export interface CondClause {
  readonly condition: Expression;
  readonly body: Expression;
}

export interface CaseExpression {
  readonly scrutinee: Expression;
  readonly clauses: ReadonlyArray<CaseClause>;
  readonly default: Expression | null;
}

export interface CaseClause {
  readonly keys: ReadonlyArray<Expression>;
  readonly body: Expression;
}

export interface AndExpression {
  readonly expressions: ReadonlyArray<Expression>;
}

export interface OrExpression {
  readonly expressions: ReadonlyArray<Expression>;
}

export interface NotExpression {
  readonly expression: Expression;
}

export interface DoExpression {
  readonly expressions: ReadonlyArray<Expression>;
}

export interface BeginExpression {
  readonly expressions: ReadonlyArray<Expression>;
}

export interface QuoteExpression {
  readonly body: Expression;
}

export interface QuasiquoteExpression {
  readonly body: Expression;
}

export interface UnquoteExpression {
  readonly body: Expression;
}

export interface SplicingUnquoteExpression {
  readonly body: Expression;
}

export interface LetExpression {
  readonly kind: LetKind;
  readonly bindings: ReadonlyArray<LetBinding>;
  readonly body: ReadonlyArray<Expression>;
}

export type LetKind =
  | { readonly tag: "parallel" }
  | { readonly tag: "sequential" }
  | { readonly tag: "recursive" };

export type LetBinding =
  | { readonly tag: "simple"; readonly value: SimpleBinding }
  | { readonly tag: "destructuring"; readonly value: DestructuringBinding };

export interface SimpleBinding {
  readonly name: Symbol;
  readonly value: Expression;
}

export interface DestructuringBinding {
  readonly pattern: DestructuringPattern;
  readonly value: Expression;
}

export type DestructuringPattern =
  | { readonly tag: "sequential"; readonly value: ReadonlyArray<Symbol> }
  | { readonly tag: "associative"; readonly value: ReadonlyArray<Symbol> }
  | { readonly tag: "rest"; readonly value: ReadonlyArray<Symbol> };

export type Pattern =
  | { readonly tag: "constructor"; readonly value: ConstructorPattern }
  | { readonly tag: "literal"; readonly value: LiteralPattern }
  | { readonly tag: "wildcard"; readonly value: WildcardPattern }
  | { readonly tag: "variable"; readonly value: Symbol };

export interface ConstructorPattern {
  readonly constructor: Symbol;
  readonly arguments: ReadonlyArray<Pattern>;
}

export interface LiteralPattern {
  readonly value: Literal;
}

export interface WildcardPattern {

}

export type Literal =
  | { readonly tag: "integer"; readonly value: IntegerLiteral }
  | { readonly tag: "float"; readonly value: FloatLiteral }
  | { readonly tag: "string"; readonly value: string }
  | { readonly tag: "character"; readonly value: CharacterLiteral }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "nil" }
  | { readonly tag: "keyword"; readonly value: Keyword }
  | { readonly tag: "symbol"; readonly value: Symbol };

export interface IntegerLiteral {
  readonly value: bigint;
  readonly bigint: boolean;
}

export interface FloatLiteral {
  readonly value: number;
  readonly precision: string | null;
}

export interface CharacterLiteral {
  readonly value: string;
}

export type BooleanStyle =
  | { readonly tag: "trueFalse" }
  | { readonly tag: "tNil" }
  | { readonly tag: "hashTF" };

export type NilStyle =
  | { readonly tag: "nil" }
  | { readonly tag: "emptyList" };

export type Symbol = string & { readonly __brand: "Symbol" };

export interface Keyword {
  readonly name: string;
  readonly namespace: string | null;
}

export interface QualifiedSymbol {
  readonly namespace: string;
  readonly name: string;
}

export type NamespaceName = string & { readonly __brand: "NamespaceName" };

export interface ListLiteral {
  readonly elements: ReadonlyArray<Expression>;
  readonly quoted: boolean;
}

export interface VectorLiteral {
  readonly elements: ReadonlyArray<Expression>;
}

export interface MapLiteral {
  readonly entries: ReadonlyArray<MapEntry>;
}

export interface MapEntry {
  readonly key: Expression;
  readonly value: Expression;
}

export interface SetLiteral {
  readonly elements: ReadonlyArray<Expression>;
}

export interface ConsExpression {
  readonly head: Expression;
  readonly tail: Expression;
}

export interface DottedPair {
  readonly car: Expression;
  readonly cdr: Expression;
}

export interface TypeHint {
  readonly name: Symbol;
  readonly type: TypeSpecifier;
}

export type TypeSpecifier =
  | { readonly tag: "named"; readonly value: Symbol }
  | { readonly tag: "list"; readonly value: TypeSpecifier }
  | { readonly tag: "function"; readonly value: ReadonlyArray<TypeSpecifier> }
  | { readonly tag: "maybe"; readonly value: TypeSpecifier }
  | { readonly tag: "map"; readonly value: ReadonlyArray<TypeSpecifier> }
  | { readonly tag: "set"; readonly value: TypeSpecifier }
  | { readonly tag: "pair"; readonly value: ReadonlyArray<TypeSpecifier> }
  | { readonly tag: "either"; readonly value: ReadonlyArray<TypeSpecifier> }
  | { readonly tag: "unit" };

export interface ModuleDeclaration {
  readonly name: NamespaceName;
  readonly doc: Docstring | null;
}

export interface ImportDeclaration {
  readonly module: NamespaceName;
  readonly spec: ImportSpec;
}

export type ImportSpec =
  | { readonly tag: "all" }
  | { readonly tag: "alias"; readonly value: Symbol }
  | { readonly tag: "only"; readonly value: ReadonlyArray<Symbol> }
  | { readonly tag: "rename"; readonly value: ReadonlyArray<ReadonlyArray<Symbol>> };

export interface ExportDeclaration {
  readonly symbols: ReadonlyArray<Symbol>;
}

export interface Comment {
  readonly style: CommentStyle;
  readonly text: string;
}

export type CommentStyle =
  | { readonly tag: "line" }
  | { readonly tag: "block" }
  | { readonly tag: "datum" };

export type Docstring = string & { readonly __brand: "Docstring" };

export type Dialect =
  | { readonly tag: "clojure" }
  | { readonly tag: "emacsLisp" }
  | { readonly tag: "commonLisp" }
  | { readonly tag: "scheme" };

export type SExpression =
  | { readonly tag: "atom"; readonly value: string }
  | { readonly tag: "list"; readonly value: ReadonlyArray<SExpression> };
