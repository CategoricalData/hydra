(* Error types for module and package validation *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.packaging.
Record ConflictingModuleNamespaceError : Type := Build_ConflictingModuleNamespaceError {
conflictingModuleNamespaceError_first : Namespace ;
conflictingModuleNamespaceError_second : Namespace ;
}.

Record ConflictingVariantNameError : Type := Build_ConflictingVariantNameError {
conflictingVariantNameError_namespace : Namespace ;
conflictingVariantNameError_typeName : Name ;
conflictingVariantNameError_variantName : Name ;
conflictingVariantNameError_conflictingName : Name ;
}.

Record DefinitionNotInModuleNamespaceError : Type := Build_DefinitionNotInModuleNamespaceError {
definitionNotInModuleNamespaceError_namespace : Namespace ;
definitionNotInModuleNamespaceError_name : Name ;
}.

Record DuplicateDefinitionNameError : Type := Build_DuplicateDefinitionNameError {
duplicateDefinitionNameError_namespace : Namespace ;
duplicateDefinitionNameError_name : Name ;
}.

Record DuplicateModuleNamespaceError : Type := Build_DuplicateModuleNamespaceError {
duplicateModuleNamespaceError_namespace : Namespace ;
}.

Inductive InvalidModuleError : Type :=
| InvalidModuleError_ConflictingVariantName : forall (_ : ConflictingVariantNameError) , InvalidModuleError
| InvalidModuleError_DefinitionNotInModuleNamespace : forall (_ : DefinitionNotInModuleNamespaceError) , InvalidModuleError
| InvalidModuleError_DuplicateDefinitionName : forall (_ : DuplicateDefinitionNameError) , InvalidModuleError.

Inductive InvalidPackageError : Type :=
| InvalidPackageError_ConflictingModuleNamespace : forall (_ : ConflictingModuleNamespaceError) , InvalidPackageError
| InvalidPackageError_DuplicateModuleNamespace : forall (_ : DuplicateModuleNamespaceError) , InvalidPackageError
| InvalidPackageError_InvalidModule : forall (_ : InvalidModuleError) , InvalidPackageError.

