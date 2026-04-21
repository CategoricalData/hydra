// Note: this is an automatically generated file. Do not edit.

/**
 * Hydra's core data model, consisting of the fundamental hydra.core.Term type and all of its dependencies.
 */



export interface AnnotatedTerm {
  readonly body: Term;
  readonly annotation: ReadonlyMap<Name, Term>;
}

export interface AnnotatedType {
  readonly body: Type;
  readonly annotation: ReadonlyMap<Name, Term>;
}

export interface Application {
  readonly function: Term;
  readonly argument: Term;
}

export interface ApplicationType {
  readonly function: Type;
  readonly argument: Type;
}

export interface Binding {
  readonly name: Name;
  readonly term: Term;
  readonly type: TypeScheme | null;
}

export interface CaseStatement {
  readonly typeName: Name;
  readonly default: Term | null;
  readonly cases: ReadonlyArray<Field>;
}

export interface EitherType {
  readonly left: Type;
  readonly right: Type;
}

export interface PairType {
  readonly first: Type;
  readonly second: Type;
}

export interface Field {
  readonly name: Name;
  readonly term: Term;
}

export interface FieldType {
  readonly name: Name;
  readonly type: Type;
}

export type FloatType =
  | { readonly tag: "bigfloat" }
  | { readonly tag: "float32" }
  | { readonly tag: "float64" };

export type FloatValue =
  | { readonly tag: "bigfloat"; readonly value: number }
  | { readonly tag: "float32"; readonly value: number }
  | { readonly tag: "float64"; readonly value: number };

export interface ForallType {
  readonly parameter: Name;
  readonly body: Type;
}

export interface FunctionType {
  readonly domain: Type;
  readonly codomain: Type;
}

export interface Injection {
  readonly typeName: Name;
  readonly field: Field;
}

export type IntegerType =
  | { readonly tag: "bigint" }
  | { readonly tag: "int8" }
  | { readonly tag: "int16" }
  | { readonly tag: "int32" }
  | { readonly tag: "int64" }
  | { readonly tag: "uint8" }
  | { readonly tag: "uint16" }
  | { readonly tag: "uint32" }
  | { readonly tag: "uint64" };

export type IntegerValue =
  | { readonly tag: "bigint"; readonly value: bigint }
  | { readonly tag: "int8"; readonly value: number }
  | { readonly tag: "int16"; readonly value: bigint }
  | { readonly tag: "int32"; readonly value: number }
  | { readonly tag: "int64"; readonly value: bigint }
  | { readonly tag: "uint8"; readonly value: bigint }
  | { readonly tag: "uint16"; readonly value: number }
  | { readonly tag: "uint32"; readonly value: bigint }
  | { readonly tag: "uint64"; readonly value: bigint };

export interface Lambda {
  readonly parameter: Name;
  readonly domain: Type | null;
  readonly body: Term;
}

export interface Let {
  readonly bindings: ReadonlyArray<Binding>;
  readonly body: Term;
}

export type Literal =
  | { readonly tag: "binary"; readonly value: Uint8Array }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "float"; readonly value: FloatValue }
  | { readonly tag: "integer"; readonly value: IntegerValue }
  | { readonly tag: "string"; readonly value: string };

export type LiteralType =
  | { readonly tag: "binary" }
  | { readonly tag: "boolean" }
  | { readonly tag: "float"; readonly value: FloatType }
  | { readonly tag: "integer"; readonly value: IntegerType }
  | { readonly tag: "string" };

export interface MapType {
  readonly keys: Type;
  readonly values: Type;
}

export type Name = string & { readonly __brand: "Name" };

export interface Projection {
  readonly typeName: Name;
  readonly field: Name;
}

export interface Record {
  readonly typeName: Name;
  readonly fields: ReadonlyArray<Field>;
}

export type Term =
  | { readonly tag: "annotated"; readonly value: AnnotatedTerm }
  | { readonly tag: "application"; readonly value: Application }
  | { readonly tag: "cases"; readonly value: CaseStatement }
  | { readonly tag: "either"; readonly value: Term | Term }
  | { readonly tag: "inject"; readonly value: Injection }
  | { readonly tag: "lambda"; readonly value: Lambda }
  | { readonly tag: "let"; readonly value: Let }
  | { readonly tag: "list"; readonly value: ReadonlyArray<Term> }
  | { readonly tag: "literal"; readonly value: Literal }
  | { readonly tag: "map"; readonly value: ReadonlyMap<Term, Term> }
  | { readonly tag: "maybe"; readonly value: Term | null }
  | { readonly tag: "pair"; readonly value: readonly [Term, Term] }
  | { readonly tag: "project"; readonly value: Projection }
  | { readonly tag: "record"; readonly value: Record }
  | { readonly tag: "set"; readonly value: ReadonlySet<Term> }
  | { readonly tag: "typeApplication"; readonly value: TypeApplicationTerm }
  | { readonly tag: "typeLambda"; readonly value: TypeLambda }
  | { readonly tag: "unit" }
  | { readonly tag: "unwrap"; readonly value: Name }
  | { readonly tag: "variable"; readonly value: Name }
  | { readonly tag: "wrap"; readonly value: WrappedTerm };

export type Type =
  | { readonly tag: "annotated"; readonly value: AnnotatedType }
  | { readonly tag: "application"; readonly value: ApplicationType }
  | { readonly tag: "either"; readonly value: EitherType }
  | { readonly tag: "forall"; readonly value: ForallType }
  | { readonly tag: "function"; readonly value: FunctionType }
  | { readonly tag: "list"; readonly value: Type }
  | { readonly tag: "literal"; readonly value: LiteralType }
  | { readonly tag: "map"; readonly value: MapType }
  | { readonly tag: "maybe"; readonly value: Type }
  | { readonly tag: "pair"; readonly value: PairType }
  | { readonly tag: "record"; readonly value: ReadonlyArray<FieldType> }
  | { readonly tag: "set"; readonly value: Type }
  | { readonly tag: "union"; readonly value: ReadonlyArray<FieldType> }
  | { readonly tag: "unit" }
  | { readonly tag: "variable"; readonly value: Name }
  | { readonly tag: "void" }
  | { readonly tag: "wrap"; readonly value: Type };

export interface TypeApplicationTerm {
  readonly body: Term;
  readonly type: Type;
}

export interface TypeLambda {
  readonly parameter: Name;
  readonly body: Term;
}

export interface TypeScheme {
  readonly variables: ReadonlyArray<Name>;
  readonly type: Type;
  readonly constraints: ReadonlyMap<Name, TypeVariableMetadata> | null;
}

export interface TypeVariableMetadata {
  readonly classes: ReadonlySet<Name>;
}

export interface WrappedTerm {
  readonly typeName: Name;
  readonly body: Term;
}
