// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.haskell;

/**
 * DSL functions for hydra.haskell.environment
 */
public interface Environment {
  static hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> haskellModuleMetadata(hydra.phantoms.TTerm<Boolean> usesByteString, hydra.phantoms.TTerm<Boolean> usesInt, hydra.phantoms.TTerm<Boolean> usesMap, hydra.phantoms.TTerm<Boolean> usesSet) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("usesByteString"), (usesByteString).value),
      new hydra.core.Field(new hydra.core.Name("usesInt"), (usesInt).value),
      new hydra.core.Field(new hydra.core.Name("usesMap"), (usesMap).value),
      new hydra.core.Field(new hydra.core.Name("usesSet"), (usesSet).value)))));
  }

  static hydra.phantoms.TTerm<Boolean> haskellModuleMetadataUsesByteString(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesByteString"))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> haskellModuleMetadataUsesInt(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesInt"))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> haskellModuleMetadataUsesMap(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesMap"))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> haskellModuleMetadataUsesSet(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesSet"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> haskellModuleMetadataWithUsesByteString(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("usesByteString"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("usesInt"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesInt"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesMap"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesMap"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesSet"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesSet"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> haskellModuleMetadataWithUsesInt(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("usesByteString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesByteString"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesInt"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("usesMap"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesMap"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesSet"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesSet"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> haskellModuleMetadataWithUsesMap(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("usesByteString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesByteString"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesInt"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesInt"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesMap"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("usesSet"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesSet"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> haskellModuleMetadataWithUsesSet(hydra.phantoms.TTerm<hydra.haskell.environment.HaskellModuleMetadata> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("usesByteString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesByteString"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesInt"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesInt"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesMap"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata"), new hydra.core.Name("usesMap"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("usesSet"), (newVal).value)))));
  }
}
