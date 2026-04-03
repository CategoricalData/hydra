// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.packaging
 */
public interface Packaging {
  static hydra.phantoms.TTerm<hydra.packaging.Definition> definitionTerm(hydra.phantoms.TTerm<hydra.packaging.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.packaging.Definition"), new hydra.core.Field(new hydra.core.Name("term"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Definition> definitionType(hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.packaging.Definition"), new hydra.core.Field(new hydra.core.Name("type"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.FileExtension> fileExtension(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.packaging.FileExtension"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Library> library(hydra.phantoms.TTerm<hydra.packaging.Namespace> namespace, hydra.phantoms.TTerm<String> prefix, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.graph.Primitive>> primitives) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("prefix"), (prefix).value),
      new hydra.core.Field(new hydra.core.Name("primitives"), (primitives).value)))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Namespace> libraryNamespace(hydra.phantoms.TTerm<hydra.packaging.Library> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> libraryPrefix(hydra.phantoms.TTerm<hydra.packaging.Library> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("prefix"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.graph.Primitive>> libraryPrimitives(hydra.phantoms.TTerm<hydra.packaging.Library> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("primitives"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Library> libraryWithNamespace(hydra.phantoms.TTerm<hydra.packaging.Library> original, hydra.phantoms.TTerm<hydra.packaging.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("prefix"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("prefix"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("primitives"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Library> libraryWithPrefix(hydra.phantoms.TTerm<hydra.packaging.Library> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("prefix"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("primitives"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Library> libraryWithPrimitives(hydra.phantoms.TTerm<hydra.packaging.Library> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.graph.Primitive>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("prefix"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Library"), new hydra.core.Name("prefix"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Module> module(hydra.phantoms.TTerm<hydra.packaging.Namespace> namespace, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Definition>> definitions, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Namespace>> termDependencies, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Namespace>> typeDependencies, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("definitions"), (definitions).value),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), (termDependencies).value),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), (typeDependencies).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Definition>> moduleDefinitions(hydra.phantoms.TTerm<hydra.packaging.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("definitions"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> moduleDescription(hydra.phantoms.TTerm<hydra.packaging.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("description"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Namespace> moduleNamespace(hydra.phantoms.TTerm<hydra.packaging.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Namespace>> moduleTermDependencies(hydra.phantoms.TTerm<hydra.packaging.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("termDependencies"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Namespace>> moduleTypeDependencies(hydra.phantoms.TTerm<hydra.packaging.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("typeDependencies"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Module> moduleWithDefinitions(hydra.phantoms.TTerm<hydra.packaging.Module> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Definition>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Module> moduleWithDescription(hydra.phantoms.TTerm<hydra.packaging.Module> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Module> moduleWithNamespace(hydra.phantoms.TTerm<hydra.packaging.Module> original, hydra.phantoms.TTerm<hydra.packaging.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Module> moduleWithTermDependencies(hydra.phantoms.TTerm<hydra.packaging.Module> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Namespace>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Module> moduleWithTypeDependencies(hydra.phantoms.TTerm<hydra.packaging.Module> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Namespace>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Namespace> namespace(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.packaging.Namespace"), (x).value)));
  }

  static <N> hydra.phantoms.TTerm<hydra.packaging.Namespaces<N>> namespaces(hydra.phantoms.TTerm<hydra.util.Pair<hydra.packaging.Namespace, N>> focus, hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.packaging.Namespace, N>> mapping) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Namespaces"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("focus"), (focus).value),
      new hydra.core.Field(new hydra.core.Name("mapping"), (mapping).value)))));
  }

  static <N> hydra.phantoms.TTerm<hydra.util.Pair<hydra.packaging.Namespace, N>> namespacesFocus(hydra.phantoms.TTerm<hydra.packaging.Namespaces<N>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Namespaces"), new hydra.core.Name("focus"))))), (x).value)));
  }

  static <N> hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.packaging.Namespace, N>> namespacesMapping(hydra.phantoms.TTerm<hydra.packaging.Namespaces<N>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Namespaces"), new hydra.core.Name("mapping"))))), (x).value)));
  }

  static <N> hydra.phantoms.TTerm<hydra.packaging.Namespaces<N>> namespacesWithFocus(hydra.phantoms.TTerm<hydra.packaging.Namespaces<N>> original, hydra.phantoms.TTerm<hydra.util.Pair<hydra.packaging.Namespace, N>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Namespaces"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("focus"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("mapping"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Namespaces"), new hydra.core.Name("mapping"))))), (original).value)))))));
  }

  static <N> hydra.phantoms.TTerm<hydra.packaging.Namespaces<N>> namespacesWithMapping(hydra.phantoms.TTerm<hydra.packaging.Namespaces<N>> original, hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.packaging.Namespace, N>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Namespaces"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("focus"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.Namespaces"), new hydra.core.Name("focus"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("mapping"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.Package_> package_(hydra.phantoms.TTerm<hydra.packaging.PackageName> name, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Module>> modules, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.PackageName>> dependencies, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description) {
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

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Module>> packageModules(hydra.phantoms.TTerm<hydra.packaging.Package_> x) {
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

  static hydra.phantoms.TTerm<hydra.packaging.Package_> packageWithModules(hydra.phantoms.TTerm<hydra.packaging.Package_> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.packaging.Module>> newVal) {
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

  static hydra.phantoms.TTerm<hydra.packaging.QualifiedName> qualifiedName(hydra.phantoms.TTerm<hydra.util.Maybe<hydra.packaging.Namespace>> namespace, hydra.phantoms.TTerm<String> local) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.QualifiedName"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("local"), (local).value)))));
  }

  static hydra.phantoms.TTerm<String> qualifiedNameLocal(hydra.phantoms.TTerm<hydra.packaging.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.QualifiedName"), new hydra.core.Name("local"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.packaging.Namespace>> qualifiedNameNamespace(hydra.phantoms.TTerm<hydra.packaging.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.QualifiedName"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.QualifiedName> qualifiedNameWithLocal(hydra.phantoms.TTerm<hydra.packaging.QualifiedName> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.QualifiedName"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.QualifiedName"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("local"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.QualifiedName> qualifiedNameWithNamespace(hydra.phantoms.TTerm<hydra.packaging.QualifiedName> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.packaging.Namespace>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.QualifiedName"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("local"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.QualifiedName"), new hydra.core.Name("local"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.TermDefinition> termDefinition(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.Term> term, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("term"), (term).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> termDefinitionName(hydra.phantoms.TTerm<hydra.packaging.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termDefinitionTerm(hydra.phantoms.TTerm<hydra.packaging.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("term"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> termDefinitionType(hydra.phantoms.TTerm<hydra.packaging.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.TermDefinition> termDefinitionWithName(hydra.phantoms.TTerm<hydra.packaging.TermDefinition> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.TermDefinition> termDefinitionWithTerm(hydra.phantoms.TTerm<hydra.packaging.TermDefinition> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.TermDefinition> termDefinitionWithType(hydra.phantoms.TTerm<hydra.packaging.TermDefinition> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TermDefinition"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> typeDefinition(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.TypeScheme> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TypeDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> typeDefinitionName(hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TypeDefinition"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeScheme> typeDefinitionType(hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TypeDefinition"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> typeDefinitionWithName(hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TypeDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TypeDefinition"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> typeDefinitionWithType(hydra.phantoms.TTerm<hydra.packaging.TypeDefinition> original, hydra.phantoms.TTerm<hydra.core.TypeScheme> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TypeDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.packaging.TypeDefinition"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<String> unFileExtension(hydra.phantoms.TTerm<hydra.packaging.FileExtension> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.packaging.FileExtension")))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unNamespace(hydra.phantoms.TTerm<hydra.packaging.Namespace> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.packaging.Namespace")))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unPackageName(hydra.phantoms.TTerm<hydra.packaging.PackageName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.packaging.PackageName")))), (x).value)));
  }
}
