// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.error;

/**
 * DSL functions for hydra.error.packaging
 */
public interface Packaging {
  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> conflictingModuleNamespaceError(hydra.phantoms.TTerm<hydra.module.Namespace> first, hydra.phantoms.TTerm<hydra.module.Namespace> second) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("first"), (first).value),
      new hydra.core.Field(new hydra.core.Name("second"), (second).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> conflictingModuleNamespaceErrorFirst(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), new hydra.core.Name("first"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> conflictingModuleNamespaceErrorSecond(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), new hydra.core.Name("second"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> conflictingModuleNamespaceErrorWithFirst(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("first"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), new hydra.core.Name("second"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> conflictingModuleNamespaceErrorWithSecond(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), new hydra.core.Name("first"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("second"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> conflictingVariantNameError(hydra.phantoms.TTerm<hydra.module.Namespace> namespace, hydra.phantoms.TTerm<hydra.core.Name> typeName, hydra.phantoms.TTerm<hydra.core.Name> variantName, hydra.phantoms.TTerm<hydra.core.Name> conflictingName) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value),
      new hydra.core.Field(new hydra.core.Name("variantName"), (variantName).value),
      new hydra.core.Field(new hydra.core.Name("conflictingName"), (conflictingName).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> conflictingVariantNameErrorConflictingName(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("conflictingName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> conflictingVariantNameErrorNamespace(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> conflictingVariantNameErrorTypeName(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("typeName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> conflictingVariantNameErrorVariantName(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("variantName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> conflictingVariantNameErrorWithConflictingName(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("typeName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("variantName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("variantName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("conflictingName"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> conflictingVariantNameErrorWithNamespace(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("typeName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("variantName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("variantName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("conflictingName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("conflictingName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> conflictingVariantNameErrorWithTypeName(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("variantName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("variantName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("conflictingName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("conflictingName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> conflictingVariantNameErrorWithVariantName(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("typeName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("variantName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("conflictingName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), new hydra.core.Name("conflictingName"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> definitionNotInModuleNamespaceError(hydra.phantoms.TTerm<hydra.module.Namespace> namespace, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> definitionNotInModuleNamespaceErrorName(hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> definitionNotInModuleNamespaceErrorNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> definitionNotInModuleNamespaceErrorWithName(hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> definitionNotInModuleNamespaceErrorWithNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> duplicateDefinitionNameError(hydra.phantoms.TTerm<hydra.module.Namespace> namespace, hydra.phantoms.TTerm<hydra.core.Name> name) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> duplicateDefinitionNameErrorName(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> duplicateDefinitionNameErrorNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> duplicateDefinitionNameErrorWithName(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> duplicateDefinitionNameErrorWithNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), new hydra.core.Name("name"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DuplicateModuleNamespaceError> duplicateModuleNamespaceError(hydra.phantoms.TTerm<hydra.module.Namespace> namespace) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> duplicateModuleNamespaceErrorNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.DuplicateModuleNamespaceError> duplicateModuleNamespaceErrorWithNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateModuleNamespaceError> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.InvalidModuleError> invalidModuleErrorConflictingVariantName(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingVariantNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.packaging.InvalidModuleError"), new hydra.core.Field(new hydra.core.Name("conflictingVariantName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.InvalidModuleError> invalidModuleErrorDefinitionNotInModuleNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DefinitionNotInModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.packaging.InvalidModuleError"), new hydra.core.Field(new hydra.core.Name("definitionNotInModuleNamespace"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.InvalidModuleError> invalidModuleErrorDuplicateDefinitionName(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateDefinitionNameError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.packaging.InvalidModuleError"), new hydra.core.Field(new hydra.core.Name("duplicateDefinitionName"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.InvalidPackageError> invalidPackageErrorConflictingModuleNamespace(hydra.phantoms.TTerm<hydra.error.packaging.ConflictingModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.packaging.InvalidPackageError"), new hydra.core.Field(new hydra.core.Name("conflictingModuleNamespace"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.InvalidPackageError> invalidPackageErrorDuplicateModuleNamespace(hydra.phantoms.TTerm<hydra.error.packaging.DuplicateModuleNamespaceError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.packaging.InvalidPackageError"), new hydra.core.Field(new hydra.core.Name("duplicateModuleNamespace"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.error.packaging.InvalidPackageError> invalidPackageErrorInvalidModule(hydra.phantoms.TTerm<hydra.error.packaging.InvalidModuleError> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.packaging.InvalidPackageError"), new hydra.core.Field(new hydra.core.Name("invalidModule"), (x).value))));
  }
}
