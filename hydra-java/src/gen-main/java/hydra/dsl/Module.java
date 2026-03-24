// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.module
 */
public interface Module {
  static hydra.phantoms.TTerm<hydra.module.Definition> definitionTerm(hydra.phantoms.TTerm<hydra.module.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.module.Definition"), new hydra.core.Field(new hydra.core.Name("term"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.module.Definition> definitionType(hydra.phantoms.TTerm<hydra.module.TypeDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.module.Definition"), new hydra.core.Field(new hydra.core.Name("type"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.module.FileExtension> fileExtension(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.module.FileExtension"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unFileExtension(hydra.phantoms.TTerm<hydra.module.FileExtension> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.module.FileExtension")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.Library> library(hydra.phantoms.TTerm<hydra.module.Namespace> namespace, hydra.phantoms.TTerm<String> prefix, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.graph.Primitive>> primitives) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("prefix"), (prefix).value),
      new hydra.core.Field(new hydra.core.Name("primitives"), (primitives).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> libraryNamespace(hydra.phantoms.TTerm<hydra.module.Library> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> libraryPrefix(hydra.phantoms.TTerm<hydra.module.Library> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("prefix"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.graph.Primitive>> libraryPrimitives(hydra.phantoms.TTerm<hydra.module.Library> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("primitives"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.Library> libraryWithNamespace(hydra.phantoms.TTerm<hydra.module.Library> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("prefix"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("prefix"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("primitives"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.Library> libraryWithPrefix(hydra.phantoms.TTerm<hydra.module.Library> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("prefix"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("primitives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("primitives"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.Library> libraryWithPrimitives(hydra.phantoms.TTerm<hydra.module.Library> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.graph.Primitive>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Library"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("prefix"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Library"), new hydra.core.Name("prefix"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("primitives"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.Module> module(hydra.phantoms.TTerm<hydra.module.Namespace> namespace, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Definition>> definitions, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Namespace>> termDependencies, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Namespace>> typeDependencies, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("definitions"), (definitions).value),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), (termDependencies).value),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), (typeDependencies).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> moduleNamespace(hydra.phantoms.TTerm<hydra.module.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Definition>> moduleDefinitions(hydra.phantoms.TTerm<hydra.module.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("definitions"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Namespace>> moduleTermDependencies(hydra.phantoms.TTerm<hydra.module.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("termDependencies"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Namespace>> moduleTypeDependencies(hydra.phantoms.TTerm<hydra.module.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("typeDependencies"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> moduleDescription(hydra.phantoms.TTerm<hydra.module.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("description"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.Module> moduleWithNamespace(hydra.phantoms.TTerm<hydra.module.Module> original, hydra.phantoms.TTerm<hydra.module.Namespace> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.Module> moduleWithDefinitions(hydra.phantoms.TTerm<hydra.module.Module> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Definition>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.Module> moduleWithTermDependencies(hydra.phantoms.TTerm<hydra.module.Module> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Namespace>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.Module> moduleWithTypeDependencies(hydra.phantoms.TTerm<hydra.module.Module> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.module.Namespace>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("description"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.Module> moduleWithDescription(hydra.phantoms.TTerm<hydra.module.Module> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Module"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("definitions"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("termDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Module"), new hydra.core.Name("typeDependencies"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.Namespace> namespace(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.module.Namespace"), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unNamespace(hydra.phantoms.TTerm<hydra.module.Namespace> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.module.Namespace")))), (x).value)));
  }

  static <N> hydra.phantoms.TTerm<hydra.module.Namespaces<N>> namespaces(hydra.phantoms.TTerm<hydra.util.Pair<hydra.module.Namespace, N>> focus, hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.module.Namespace, N>> mapping) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Namespaces"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("focus"), (focus).value),
      new hydra.core.Field(new hydra.core.Name("mapping"), (mapping).value)))));
  }

  static <N> hydra.phantoms.TTerm<hydra.util.Pair<hydra.module.Namespace, N>> namespacesFocus(hydra.phantoms.TTerm<hydra.module.Namespaces<N>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Namespaces"), new hydra.core.Name("focus"))))), (x).value)));
  }

  static <N> hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.module.Namespace, N>> namespacesMapping(hydra.phantoms.TTerm<hydra.module.Namespaces<N>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Namespaces"), new hydra.core.Name("mapping"))))), (x).value)));
  }

  static <N> hydra.phantoms.TTerm<hydra.module.Namespaces<N>> namespacesWithFocus(hydra.phantoms.TTerm<hydra.module.Namespaces<N>> original, hydra.phantoms.TTerm<hydra.util.Pair<hydra.module.Namespace, N>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Namespaces"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("focus"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("mapping"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Namespaces"), new hydra.core.Name("mapping"))))), (original).value)))))));
  }

  static <N> hydra.phantoms.TTerm<hydra.module.Namespaces<N>> namespacesWithMapping(hydra.phantoms.TTerm<hydra.module.Namespaces<N>> original, hydra.phantoms.TTerm<hydra.util.PersistentMap<hydra.module.Namespace, N>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Namespaces"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("focus"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.Namespaces"), new hydra.core.Name("focus"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("mapping"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.QualifiedName> qualifiedName(hydra.phantoms.TTerm<hydra.util.Maybe<hydra.module.Namespace>> namespace, hydra.phantoms.TTerm<String> local) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.QualifiedName"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (namespace).value),
      new hydra.core.Field(new hydra.core.Name("local"), (local).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.module.Namespace>> qualifiedNameNamespace(hydra.phantoms.TTerm<hydra.module.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.QualifiedName"), new hydra.core.Name("namespace"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> qualifiedNameLocal(hydra.phantoms.TTerm<hydra.module.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.QualifiedName"), new hydra.core.Name("local"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.QualifiedName> qualifiedNameWithNamespace(hydra.phantoms.TTerm<hydra.module.QualifiedName> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.module.Namespace>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.QualifiedName"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("local"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.QualifiedName"), new hydra.core.Name("local"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.QualifiedName> qualifiedNameWithLocal(hydra.phantoms.TTerm<hydra.module.QualifiedName> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.QualifiedName"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.QualifiedName"), new hydra.core.Name("namespace"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("local"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.TermDefinition> termDefinition(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.Term> term, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("term"), (term).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> termDefinitionName(hydra.phantoms.TTerm<hydra.module.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termDefinitionTerm(hydra.phantoms.TTerm<hydra.module.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("term"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> termDefinitionType(hydra.phantoms.TTerm<hydra.module.TermDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.TermDefinition> termDefinitionWithName(hydra.phantoms.TTerm<hydra.module.TermDefinition> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.TermDefinition> termDefinitionWithTerm(hydra.phantoms.TTerm<hydra.module.TermDefinition> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.TermDefinition> termDefinitionWithType(hydra.phantoms.TTerm<hydra.module.TermDefinition> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TermDefinition"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.module.TypeDefinition> typeDefinition(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TypeDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> typeDefinitionName(hydra.phantoms.TTerm<hydra.module.TypeDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TypeDefinition"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeDefinitionType(hydra.phantoms.TTerm<hydra.module.TypeDefinition> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TypeDefinition"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.TypeDefinition> typeDefinitionWithName(hydra.phantoms.TTerm<hydra.module.TypeDefinition> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TypeDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TypeDefinition"), new hydra.core.Name("type"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.module.TypeDefinition> typeDefinitionWithType(hydra.phantoms.TTerm<hydra.module.TypeDefinition> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TypeDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.module.TypeDefinition"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }
}
