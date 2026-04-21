// Note: this is an automatically generated file. Do not edit.

/**
 * Error types for type checking
 */



import * as Core from "../core.js";
import * as Paths from "../paths.js";
import * as Typing from "../typing.js";
import * as Variants from "../variants.js";

export type CheckingError =
  | { readonly tag: "incorrectUnification"; readonly value: IncorrectUnificationError }
  | { readonly tag: "notAForallType"; readonly value: NotAForallTypeError }
  | { readonly tag: "notAFunctionType"; readonly value: NotAFunctionTypeError }
  | { readonly tag: "other"; readonly value: OtherCheckingError }
  | { readonly tag: "typeArityMismatch"; readonly value: TypeArityMismatchError }
  | { readonly tag: "typeMismatch"; readonly value: TypeMismatchError }
  | { readonly tag: "unboundTypeVariables"; readonly value: UnboundTypeVariablesError }
  | { readonly tag: "undefinedTermVariable"; readonly value: UndefinedTermVariableCheckingError }
  | { readonly tag: "unequalTypes"; readonly value: UnequalTypesError }
  | { readonly tag: "unsupportedTermVariant"; readonly value: UnsupportedTermVariantError }
  | { readonly tag: "untypedLambda"; readonly value: UntypedLambdaError }
  | { readonly tag: "untypedLetBinding"; readonly value: UntypedLetBindingError }
  | { readonly tag: "untypedTermVariable"; readonly value: UntypedTermVariableCheckingError };

export interface IncorrectUnificationError {
  readonly substitution: Typing.TypeSubst;
}

export interface NotAForallTypeError {
  readonly type: Core.Type;
  readonly typeArguments: ReadonlyArray<Core.Type>;
}

export interface NotAFunctionTypeError {
  readonly type: Core.Type;
}

export interface OtherCheckingError {
  readonly path: Paths.SubtermPath;
  readonly message: string;
}

export interface TypeArityMismatchError {
  readonly type: Core.Type;
  readonly expectedArity: number;
  readonly actualArity: number;
  readonly typeArguments: ReadonlyArray<Core.Type>;
}

export interface TypeMismatchError {
  readonly expectedType: Core.Type;
  readonly actualType: Core.Type;
}

export interface UnboundTypeVariablesError {
  readonly variables: ReadonlySet<Core.Name>;
  readonly type: Core.Type;
}

export interface UndefinedTermVariableCheckingError {
  readonly path: Paths.SubtermPath;
  readonly name: Core.Name;
}

export interface UnequalTypesError {
  readonly types: ReadonlyArray<Core.Type>;
  readonly description: string;
}

export interface UnsupportedTermVariantError {
  readonly termVariant: Variants.TermVariant;
}

export interface UntypedLambdaError {

}

export interface UntypedLetBindingError {
  readonly binding: Core.Binding;
}

export interface UntypedTermVariableCheckingError {
  readonly path: Paths.SubtermPath;
  readonly name: Core.Name;
}
