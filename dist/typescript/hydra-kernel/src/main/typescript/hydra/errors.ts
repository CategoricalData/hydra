// Note: this is an automatically generated file. Do not edit.

/**
 * Top-level error types for the Hydra kernel
 */



import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as Paths from "./paths.js";
import * as Typing from "./typing.js";
import * as Variants from "./variants.js";

export type DecodingError = string & { readonly __brand: "DecodingError" };

export type EmptyListError = void;

export type Error =
  | { readonly tag: "checking"; readonly value: ErrorChecking.CheckingError }
  | { readonly tag: "decoding"; readonly value: DecodingError }
  | { readonly tag: "duplicateBinding"; readonly value: ErrorCore.DuplicateBindingError }
  | { readonly tag: "duplicateField"; readonly value: ErrorCore.DuplicateFieldError }
  | { readonly tag: "extraction"; readonly value: ExtractionError }
  | { readonly tag: "inference"; readonly value: InferenceError }
  | { readonly tag: "other"; readonly value: OtherError }
  | { readonly tag: "resolution"; readonly value: ResolutionError }
  | { readonly tag: "undefinedField"; readonly value: ErrorCore.UndefinedFieldError }
  | { readonly tag: "undefinedTermVariable"; readonly value: ErrorCore.UndefinedTermVariableError }
  | { readonly tag: "untypedTermVariable"; readonly value: ErrorCore.UntypedTermVariableError }
  | { readonly tag: "unexpectedTermVariant"; readonly value: ErrorCore.UnexpectedTermVariantError }
  | { readonly tag: "unexpectedTypeVariant"; readonly value: ErrorCore.UnexpectedTypeVariantError }
  | { readonly tag: "unification"; readonly value: UnificationError };

export type ExtractionError =
  | { readonly tag: "emptyList"; readonly value: EmptyListError }
  | { readonly tag: "multipleBindings"; readonly value: MultipleBindingsError }
  | { readonly tag: "multipleFields"; readonly value: MultipleFieldsError }
  | { readonly tag: "noMatchingField"; readonly value: NoMatchingFieldError }
  | { readonly tag: "noSuchBinding"; readonly value: NoSuchBindingError }
  | { readonly tag: "notEnoughCases"; readonly value: NotEnoughCasesError }
  | { readonly tag: "unexpectedShape"; readonly value: UnexpectedShapeError };

export type InferenceError =
  | { readonly tag: "checking"; readonly value: ErrorChecking.CheckingError }
  | { readonly tag: "other"; readonly value: OtherInferenceError }
  | { readonly tag: "unification"; readonly value: UnificationInferenceError };

export interface MultipleBindingsError {
  readonly name: Core.Name;
}

export interface MultipleFieldsError {
  readonly fieldName: Core.Name;
}

export interface NoMatchingFieldError {
  readonly fieldName: Core.Name;
}

export interface NoSuchBindingError {
  readonly name: Core.Name;
}

export interface NoSuchPrimitiveError {
  readonly name: Core.Name;
}

export type NotEnoughCasesError = void;

export type OtherError = string & { readonly __brand: "OtherError" };

export interface OtherInferenceError {
  readonly path: Paths.SubtermPath;
  readonly message: string;
}

export type OtherResolutionError = string & { readonly __brand: "OtherResolutionError" };

export type ResolutionError =
  | { readonly tag: "noSuchBinding"; readonly value: NoSuchBindingError }
  | { readonly tag: "noSuchPrimitive"; readonly value: NoSuchPrimitiveError }
  | { readonly tag: "noMatchingField"; readonly value: NoMatchingFieldError }
  | { readonly tag: "other"; readonly value: OtherResolutionError }
  | { readonly tag: "unexpectedShape"; readonly value: UnexpectedShapeError };

export interface UnexpectedShapeError {
  readonly expected: string;
  readonly actual: string;
}

export interface UnificationError {
  readonly leftType: Core.Type;
  readonly rightType: Core.Type;
  readonly message: string;
}

export interface UnificationInferenceError {
  readonly path: Paths.SubtermPath;
  readonly cause: UnificationError;
}
