// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.packaging
 */
public interface Packaging {
  static hydra.core.Term definition(hydra.packaging.Definition v1) {
    return (v1).accept(new hydra.packaging.Definition.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.packaging.Definition.Term y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.packaging.Definition"), new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.Packaging.termDefinition((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.packaging.Definition.Type y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.packaging.Definition"), new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.Packaging.typeDefinition((y).value))));
      }
    });
  }

  static hydra.core.Term fileExtension(hydra.packaging.FileExtension x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.packaging.FileExtension"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term module(hydra.packaging.Module x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Module"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), hydra.encode.Packaging.namespace((x).namespace)),
      new hydra.core.Field(new hydra.core.Name("definitions"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Packaging::definition,
        (x).definitions))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Packaging::namespace,
        (x).termDependencies))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Packaging::namespace,
        (x).typeDependencies))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).description))))));
  }

  static hydra.core.Term namespace(hydra.packaging.Namespace x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.packaging.Namespace"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static <T0> hydra.core.Term namespaces(java.util.function.Function<T0, hydra.core.Term> n, hydra.packaging.Namespaces<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Namespaces"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("focus"), new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
        hydra.encode.Packaging::namespace,
        n,
        ((java.util.function.Function<hydra.packaging.Namespaces<T0>, hydra.util.Pair<hydra.packaging.Namespace, T0>>) (projected -> projected.focus)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("mapping"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.Packaging::namespace,
        n,
        ((java.util.function.Function<hydra.packaging.Namespaces<T0>, java.util.Map<hydra.packaging.Namespace, T0>>) (projected -> projected.mapping)).apply(x)))))));
  }

  static hydra.core.Term package_(hydra.packaging.Package_ x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.Package"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Packaging.packageName((x).name)),
      new hydra.core.Field(new hydra.core.Name("modules"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Packaging::module,
        (x).modules))),
      new hydra.core.Field(new hydra.core.Name("dependencies"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Packaging::packageName,
        (x).dependencies))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).description))))));
  }

  static hydra.core.Term packageName(hydra.packaging.PackageName x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.packaging.PackageName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term qualifiedName(hydra.packaging.QualifiedName x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.QualifiedName"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        hydra.encode.Packaging::namespace,
        (x).namespace))),
      new hydra.core.Field(new hydra.core.Name("local"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).local))))));
  }

  static hydra.core.Term termDefinition(hydra.packaging.TermDefinition x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TermDefinition"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.Core.term((x).term)),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        hydra.encode.Core::typeScheme,
        (x).type))))));
  }

  static hydra.core.Term typeDefinition(hydra.packaging.TypeDefinition x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.packaging.TypeDefinition"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.Core.typeScheme((x).type)))));
  }
}
