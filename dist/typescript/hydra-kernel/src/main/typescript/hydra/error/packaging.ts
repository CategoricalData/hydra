// Note: this is an automatically generated file. Do not edit.

/**
 * Error types for module and package validation
 */



import * as Core from "../core.js";
import * as Packaging from "../packaging.js";

export interface ConflictingModuleNamespaceError {
  readonly first: Packaging.Namespace;
  readonly second: Packaging.Namespace;
}

export interface ConflictingVariantNameError {
  readonly namespace: Packaging.Namespace;
  readonly typeName: Core.Name;
  readonly variantName: Core.Name;
  readonly conflictingName: Core.Name;
}

export interface DefinitionNotInModuleNamespaceError {
  readonly namespace: Packaging.Namespace;
  readonly name: Core.Name;
}

export interface DuplicateDefinitionNameError {
  readonly namespace: Packaging.Namespace;
  readonly name: Core.Name;
}

export interface DuplicateModuleNamespaceError {
  readonly namespace: Packaging.Namespace;
}

export type InvalidModuleError =
  | { readonly tag: "conflictingVariantName"; readonly value: ConflictingVariantNameError }
  | { readonly tag: "definitionNotInModuleNamespace"; readonly value: DefinitionNotInModuleNamespaceError }
  | { readonly tag: "duplicateDefinitionName"; readonly value: DuplicateDefinitionNameError };

export type InvalidPackageError =
  | { readonly tag: "conflictingModuleNamespace"; readonly value: ConflictingModuleNamespaceError }
  | { readonly tag: "duplicateModuleNamespace"; readonly value: DuplicateModuleNamespaceError }
  | { readonly tag: "invalidModule"; readonly value: InvalidModuleError };
