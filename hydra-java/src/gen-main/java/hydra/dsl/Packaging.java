// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.packaging
 */
public interface Packaging {
  static hydra.phantoms.TTerm<hydra.packaging.Package_> package_(hydra.phantoms.TTerm<hydra.packaging.PackageName> name, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Module>> modules, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.PackageName>> dependencies, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Package"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("modules"), (modules).value),
      new hydra.core.Field(new hydra.core.Name("dependencies"), (dependencies).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.PackageName>> packageDependencies(hydra.phantoms.TTerm<hydra.packaging.Package_> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("dependencies"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> packageDescription(hydra.phantoms.TTerm<hydra.packaging.Package_> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("description"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Module>> packageModules(hydra.phantoms.TTerm<hydra.packaging.Package_> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("modules"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.PackageName> packageName(hydra.phantoms.TTerm<hydra.packaging.Package_> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.PackageName> packageName_(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.packaging.PackageName"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Package_> packageWithDependencies(hydra.phantoms.TTerm<hydra.packaging.Package_> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.PackageName>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Package"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("modules"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("modules"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("dependencies"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Package_> packageWithDescription(hydra.phantoms.TTerm<hydra.packaging.Package_> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Package"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("modules"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("modules"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("dependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("dependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Package_> packageWithModules(hydra.phantoms.TTerm<hydra.packaging.Package_> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Module>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Package"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("modules"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("dependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("dependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Package_> packageWithName(hydra.phantoms.TTerm<hydra.packaging.Package_> original, hydra.phantoms.TTerm<hydra.packaging.PackageName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Package"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("modules"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("modules"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("dependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("dependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Package"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<String> unPackageName(hydra.phantoms.TTerm<hydra.packaging.PackageName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.packaging.PackageName")))), (x).value)));
  }
}
