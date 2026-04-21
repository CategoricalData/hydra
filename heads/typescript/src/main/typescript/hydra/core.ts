/**
 * Core Hydra types for TypeScript.
 *
 * This file provides the hand-written TypeScript equivalents of the kernel types.
 * Eventually these will be replaced by generated types from the TypeScript coder,
 * but they serve as the bootstrap foundation for the runtime.
 */

// -- Branded newtypes --

export type Name = string & { readonly __brand: "Name" };

export function Name(s: string): Name {
  return s as Name;
}

// -- Maybe and Either --

export type Maybe<T> =
  | { readonly tag: "nothing" }
  | { readonly tag: "just"; readonly value: T };

export function nothing<T>(): Maybe<T> {
  return { tag: "nothing" };
}

export function just<T>(value: T): Maybe<T> {
  return { tag: "just", value };
}

export type Either<L, R> =
  | { readonly tag: "left"; readonly value: L }
  | { readonly tag: "right"; readonly value: R };

export function left<L, R>(value: L): Either<L, R> {
  return { tag: "left", value };
}

export function right<L, R>(value: R): Either<L, R> {
  return { tag: "right", value };
}

// -- Literal types --

export type FloatType =
  | { readonly tag: "bigfloat" }
  | { readonly tag: "float32" }
  | { readonly tag: "float64" };

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

export type LiteralType =
  | { readonly tag: "binary" }
  | { readonly tag: "boolean" }
  | { readonly tag: "float"; readonly value: FloatType }
  | { readonly tag: "integer"; readonly value: IntegerType }
  | { readonly tag: "string" };

export type FloatValue =
  | { readonly tag: "bigfloat"; readonly value: number }
  | { readonly tag: "float32"; readonly value: number }
  | { readonly tag: "float64"; readonly value: number };

export type IntegerValue =
  | { readonly tag: "bigint"; readonly value: bigint }
  | { readonly tag: "int8"; readonly value: number }
  | { readonly tag: "int16"; readonly value: number }
  | { readonly tag: "int32"; readonly value: number }
  | { readonly tag: "int64"; readonly value: bigint }
  | { readonly tag: "uint8"; readonly value: number }
  | { readonly tag: "uint16"; readonly value: number }
  | { readonly tag: "uint32"; readonly value: number }
  | { readonly tag: "uint64"; readonly value: bigint };

export type Literal =
  | { readonly tag: "binary"; readonly value: Uint8Array }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "float"; readonly value: FloatValue }
  | { readonly tag: "integer"; readonly value: IntegerValue }
  | { readonly tag: "string"; readonly value: string };

// -- Core type system --

export type AnnotatedType = {
  readonly body: Type;
  readonly annotation: ReadonlyMap<Name, Term>;
};

export type ApplicationType = {
  readonly function: Type;
  readonly argument: Type;
};

export type EitherType = {
  readonly left: Type;
  readonly right: Type;
};

export type FieldType = {
  readonly name: Name;
  readonly type: Type;
};

export type ForallType = {
  readonly variable: Name;
  readonly body: Type;
};

export type FunctionType = {
  readonly domain: Type;
  readonly codomain: Type;
};

export type MapType = {
  readonly keys: Type;
  readonly values: Type;
};

export type PairType = {
  readonly first: Type;
  readonly second: Type;
};

export type TypeScheme = {
  readonly variables: ReadonlyArray<Name>;
  readonly type: Type;
};

export type TypeVariableMetadata = {
  readonly description: Maybe<string>;
};

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
  | { readonly tag: "wrap"; readonly value: Name };

// -- Core term system --

export type AnnotatedTerm = {
  readonly body: Term;
  readonly annotation: ReadonlyMap<Name, Term>;
};

export type Application = {
  readonly function: Term;
  readonly argument: Term;
};

export type Binding = {
  readonly name: Name;
  readonly term: Term;
  readonly type: Maybe<TypeScheme>;
};

export type CaseStatement = {
  readonly typeName: Name;
  readonly default: Maybe<Term>;
  readonly cases: ReadonlyArray<Field>;
};

export type Field = {
  readonly name: Name;
  readonly term: Term;
};

export type Injection = {
  readonly typeName: Name;
  readonly field: Field;
};

export type Lambda = {
  readonly parameter: Name;
  readonly domain: Maybe<Type>;
  readonly body: Term;
};

export type Let = {
  readonly bindings: ReadonlyArray<Binding>;
  readonly environment: Term;
};

export type Projection = {
  readonly typeName: Name;
  readonly field: Name;
};

export type Record = {
  readonly typeName: Name;
  readonly fields: ReadonlyArray<Field>;
};

export type TypeApplicationTerm = {
  readonly function: Term;
  readonly argument: Type;
};

export type TypeLambda = {
  readonly parameter: Name;
  readonly body: Term;
};

export type WrappedTerm = {
  readonly typeName: Name;
  readonly object: Term;
};

export type Term =
  | { readonly tag: "annotated"; readonly value: AnnotatedTerm }
  | { readonly tag: "application"; readonly value: Application }
  | { readonly tag: "cases"; readonly value: CaseStatement }
  | { readonly tag: "either"; readonly value: Either<Term, Term> }
  | { readonly tag: "inject"; readonly value: Injection }
  | { readonly tag: "lambda"; readonly value: Lambda }
  | { readonly tag: "let"; readonly value: Let }
  | { readonly tag: "list"; readonly value: ReadonlyArray<Term> }
  | { readonly tag: "literal"; readonly value: Literal }
  | { readonly tag: "map"; readonly value: ReadonlyMap<Term, Term> }
  | { readonly tag: "maybe"; readonly value: Maybe<Term> }
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

// -- Graph types --

export type Language = {
  readonly name: Name;
  readonly constraints: LanguageConstraints;
};

export type LanguageConstraints = {
  readonly eliminationVariants: ReadonlySet<string>;
  readonly literalVariants: ReadonlySet<string>;
  readonly floatTypes: ReadonlySet<string>;
  readonly functionVariants: ReadonlySet<string>;
  readonly integerTypes: ReadonlySet<string>;
  readonly termVariants: ReadonlySet<string>;
  readonly typeVariants: ReadonlySet<string>;
  readonly typePredicate: (t: Type) => boolean;
};

export type Primitive = {
  readonly name: Name;
  readonly type: TypeScheme;
  readonly implementation: (...args: ReadonlyArray<Term>) => Either<string, Term>;
};

export type Graph = {
  readonly elements: ReadonlyMap<Name, Term>;
  readonly types: ReadonlyMap<Name, TypeScheme>;
  readonly primitives: ReadonlyMap<Name, Primitive>;
};
