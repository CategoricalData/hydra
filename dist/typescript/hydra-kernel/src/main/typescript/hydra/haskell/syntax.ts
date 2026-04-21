// Note: this is an automatically generated file. Do not edit.

/**
 * A Haskell syntax model, loosely based on Language.Haskell.Tools.AST
 */



import * as Core from "../core.js";

export interface Alternative {
  readonly pattern: Pattern;
  readonly rhs: CaseRhs;
  readonly binds: LocalBindings | null;
}

export type Assertion =
  | { readonly tag: "class"; readonly value: ClassAssertion }
  | { readonly tag: "tuple"; readonly value: ReadonlyArray<Assertion> };

export interface ClassAssertion {
  readonly name: Name;
  readonly types: ReadonlyArray<Type>;
}

export type CaseRhs = Expression & { readonly __brand: "CaseRhs" };

export type Constructor =
  | { readonly tag: "ordinary"; readonly value: OrdinaryConstructor }
  | { readonly tag: "record"; readonly value: RecordConstructor };

export interface OrdinaryConstructor {
  readonly name: Name;
  readonly fields: ReadonlyArray<Type>;
}

export interface RecordConstructor {
  readonly name: Name;
  readonly fields: ReadonlyArray<FieldWithComments>;
}

export interface ConstructorWithComments {
  readonly body: Constructor;
  readonly comments: string | null;
}

export interface DataDeclaration {
  readonly keyword: DataOrNewtype;
  readonly context: ReadonlyArray<Assertion>;
  readonly head: DeclarationHead;
  readonly constructors: ReadonlyArray<ConstructorWithComments>;
  readonly deriving: ReadonlyArray<Deriving>;
}

export type DataOrNewtype =
  | { readonly tag: "data" }
  | { readonly tag: "newtype" };

export interface DeclarationWithComments {
  readonly body: Declaration;
  readonly comments: string | null;
}

export type Declaration =
  | { readonly tag: "data"; readonly value: DataDeclaration }
  | { readonly tag: "type"; readonly value: TypeDeclaration }
  | { readonly tag: "valueBinding"; readonly value: ValueBinding }
  | { readonly tag: "typedBinding"; readonly value: TypedBinding };

export type DeclarationHead =
  | { readonly tag: "application"; readonly value: ApplicationDeclarationHead }
  | { readonly tag: "parens"; readonly value: DeclarationHead }
  | { readonly tag: "simple"; readonly value: Name };

export interface ApplicationDeclarationHead {
  readonly function: DeclarationHead;
  readonly operand: Variable;
}

export type Deriving = ReadonlyArray<Name> & { readonly __brand: "Deriving" };

export type Export =
  | { readonly tag: "declaration"; readonly value: ImportExportSpec }
  | { readonly tag: "module"; readonly value: ModuleName };

export type Expression =
  | { readonly tag: "application"; readonly value: ApplicationExpression }
  | { readonly tag: "case"; readonly value: CaseExpression }
  | { readonly tag: "constructRecord"; readonly value: ConstructRecordExpression }
  | { readonly tag: "do"; readonly value: ReadonlyArray<Statement> }
  | { readonly tag: "if"; readonly value: IfExpression }
  | { readonly tag: "infixApplication"; readonly value: InfixApplicationExpression }
  | { readonly tag: "literal"; readonly value: Literal }
  | { readonly tag: "lambda"; readonly value: LambdaExpression }
  | { readonly tag: "leftSection"; readonly value: SectionExpression }
  | { readonly tag: "let"; readonly value: LetExpression }
  | { readonly tag: "list"; readonly value: ReadonlyArray<Expression> }
  | { readonly tag: "parens"; readonly value: Expression }
  | { readonly tag: "prefixApplication"; readonly value: PrefixApplicationExpression }
  | { readonly tag: "rightSection"; readonly value: SectionExpression }
  | { readonly tag: "tuple"; readonly value: ReadonlyArray<Expression> }
  | { readonly tag: "typeSignature"; readonly value: TypeSignatureExpression }
  | { readonly tag: "updateRecord"; readonly value: UpdateRecordExpression }
  | { readonly tag: "variable"; readonly value: Name };

export interface ApplicationExpression {
  readonly function: Expression;
  readonly argument: Expression;
}

export interface CaseExpression {
  readonly case: Expression;
  readonly alternatives: ReadonlyArray<Alternative>;
}

export interface ConstructRecordExpression {
  readonly name: Name;
  readonly fields: ReadonlyArray<FieldUpdate>;
}

export interface IfExpression {
  readonly condition: Expression;
  readonly then: Expression;
  readonly else: Expression;
}

export interface InfixApplicationExpression {
  readonly lhs: Expression;
  readonly operator: Operator;
  readonly rhs: Expression;
}

export interface LambdaExpression {
  readonly bindings: ReadonlyArray<Pattern>;
  readonly inner: Expression;
}

export interface LetExpression {
  readonly bindings: ReadonlyArray<LocalBinding>;
  readonly inner: Expression;
}

export interface PrefixApplicationExpression {
  readonly operator: Operator;
  readonly rhs: Expression;
}

export interface SectionExpression {
  readonly operator: Operator;
  readonly expression: Expression;
}

export interface TypeSignatureExpression {
  readonly inner: Expression;
  readonly type: Type;
}

export interface UpdateRecordExpression {
  readonly inner: Expression;
  readonly fields: ReadonlyArray<FieldUpdate>;
}

export interface Field {
  readonly name: Name;
  readonly type: Type;
}

export interface FieldWithComments {
  readonly field: Field;
  readonly comments: string | null;
}

export interface FieldUpdate {
  readonly name: Name;
  readonly value: Expression;
}

export interface Import {
  readonly qualified: boolean;
  readonly module: ModuleName;
  readonly as: ModuleName | null;
  readonly spec: SpecImport | null;
}

export type SpecImport =
  | { readonly tag: "list"; readonly value: ReadonlyArray<ImportExportSpec> }
  | { readonly tag: "hiding"; readonly value: ReadonlyArray<ImportExportSpec> };

export type ImportModifier =
  | { readonly tag: "pattern" }
  | { readonly tag: "type" };

export interface ImportExportSpec {
  readonly modifier: ImportModifier | null;
  readonly name: Name;
  readonly subspec: SubspecImportExportSpec | null;
}

export type SubspecImportExportSpec =
  | { readonly tag: "all" }
  | { readonly tag: "list"; readonly value: ReadonlyArray<Name> };

export type Literal =
  | { readonly tag: "char"; readonly value: number }
  | { readonly tag: "double"; readonly value: number }
  | { readonly tag: "float"; readonly value: number }
  | { readonly tag: "int"; readonly value: number }
  | { readonly tag: "integer"; readonly value: bigint }
  | { readonly tag: "string"; readonly value: string };

export type LocalBinding =
  | { readonly tag: "signature"; readonly value: TypeSignature }
  | { readonly tag: "value"; readonly value: ValueBinding };

export type LocalBindings = ReadonlyArray<LocalBinding> & { readonly __brand: "LocalBindings" };

export interface Module {
  readonly head: ModuleHead | null;
  readonly imports: ReadonlyArray<Import>;
  readonly declarations: ReadonlyArray<DeclarationWithComments>;
}

export interface ModuleHead {
  readonly comments: string | null;
  readonly name: ModuleName;
  readonly exports: ReadonlyArray<Export>;
}

export type ModuleName = string & { readonly __brand: "ModuleName" };

export type Name =
  | { readonly tag: "implicit"; readonly value: QualifiedName }
  | { readonly tag: "normal"; readonly value: QualifiedName }
  | { readonly tag: "parens"; readonly value: QualifiedName };

export type NamePart = string & { readonly __brand: "NamePart" };

export type Operator =
  | { readonly tag: "backtick"; readonly value: QualifiedName }
  | { readonly tag: "normal"; readonly value: QualifiedName };

export type Pattern =
  | { readonly tag: "application"; readonly value: ApplicationPattern }
  | { readonly tag: "as"; readonly value: AsPattern }
  | { readonly tag: "list"; readonly value: ReadonlyArray<Pattern> }
  | { readonly tag: "literal"; readonly value: Literal }
  | { readonly tag: "name"; readonly value: Name }
  | { readonly tag: "parens"; readonly value: Pattern }
  | { readonly tag: "record"; readonly value: RecordPattern }
  | { readonly tag: "tuple"; readonly value: ReadonlyArray<Pattern> }
  | { readonly tag: "typed"; readonly value: TypedPattern }
  | { readonly tag: "wildcard" };

export interface ApplicationPattern {
  readonly name: Name;
  readonly args: ReadonlyArray<Pattern>;
}

export interface AsPattern {
  readonly name: Name;
  readonly inner: Pattern;
}

export interface RecordPattern {
  readonly name: Name;
  readonly fields: ReadonlyArray<PatternField>;
}

export interface TypedPattern {
  readonly inner: Pattern;
  readonly type: Type;
}

export interface PatternField {
  readonly name: Name;
  readonly pattern: Pattern;
}

export interface QualifiedName {
  readonly qualifiers: ReadonlyArray<NamePart>;
  readonly unqualified: NamePart;
}

export type RightHandSide = Expression & { readonly __brand: "RightHandSide" };

export type Statement = Expression & { readonly __brand: "Statement" };

export type Type =
  | { readonly tag: "application"; readonly value: ApplicationType }
  | { readonly tag: "ctx"; readonly value: ContextType }
  | { readonly tag: "function"; readonly value: FunctionType }
  | { readonly tag: "infix"; readonly value: InfixType }
  | { readonly tag: "list"; readonly value: Type }
  | { readonly tag: "parens"; readonly value: Type }
  | { readonly tag: "tuple"; readonly value: ReadonlyArray<Type> }
  | { readonly tag: "variable"; readonly value: Name };

export interface ApplicationType {
  readonly context: Type;
  readonly argument: Type;
}

export interface ContextType {
  readonly ctx: Assertion;
  readonly type: Type;
}

export interface FunctionType {
  readonly domain: Type;
  readonly codomain: Type;
}

export interface InfixType {
  readonly lhs: Type;
  readonly operator: Operator;
  readonly rhs: Operator;
}

export interface TypeDeclaration {
  readonly name: DeclarationHead;
  readonly type: Type;
}

export interface TypeSignature {
  readonly name: Name;
  readonly type: Type;
}

export interface TypedBinding {
  readonly typeSignature: TypeSignature;
  readonly valueBinding: ValueBinding;
}

export type ValueBinding =
  | { readonly tag: "simple"; readonly value: SimpleValueBinding };

export interface SimpleValueBinding {
  readonly pattern: Pattern;
  readonly rhs: RightHandSide;
  readonly localBindings: LocalBindings | null;
}

export type Variable = Name & { readonly __brand: "Variable" };
