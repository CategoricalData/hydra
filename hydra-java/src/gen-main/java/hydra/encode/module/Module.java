// Note: this is an automatically generated file. Do not edit.

package hydra.encode.module;

/**
 * Term encoders for hydra.module
 */
public interface Module {
  static hydra.core.Term definition(hydra.module.Definition v1) {
    return ((v1)).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.module.Definition.Term y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.module.Definition"), new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.module.Module.termDefinition(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.module.Definition.Type y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.module.Definition"), new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.module.Module.typeDefinition(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term fileExtension(hydra.module.FileExtension x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.module.FileExtension"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static hydra.core.Term module(hydra.module.Module x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Module"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), hydra.encode.module.Module.namespace(((x)).namespace)),
      new hydra.core.Field(new hydra.core.Name("elements"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.core.Core::binding),
        ((x)).elements))),
      new hydra.core.Field(new hydra.core.Name("termDependencies"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.module.Module::namespace),
        ((x)).termDependencies))),
      new hydra.core.Field(new hydra.core.Name("typeDependencies"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.module.Module::namespace),
        ((x)).typeDependencies))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_((x2)))),
        ((x)).description))))));
  }
  
  static hydra.core.Term namespace(hydra.module.Namespace x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.module.Namespace"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static <T0> hydra.core.Term namespaces(java.util.function.Function<T0, hydra.core.Term> n, hydra.module.Namespaces<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.Namespaces"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("focus"), new hydra.core.Term.Pair(hydra.lib.pairs.Bimap.apply(
        (hydra.encode.module.Module::namespace),
        (n),
        ((java.util.function.Function<hydra.module.Namespaces<T0>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, T0>>) (projected -> projected.focus)).apply((x))))),
      new hydra.core.Field(new hydra.core.Name("mapping"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.module.Module::namespace),
        (n),
        ((java.util.function.Function<hydra.module.Namespaces<T0>, java.util.Map<hydra.module.Namespace, T0>>) (projected -> projected.mapping)).apply((x))))))));
  }
  
  static hydra.core.Term qualifiedName(hydra.module.QualifiedName x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.QualifiedName"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("namespace"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (hydra.encode.module.Module::namespace),
        ((x)).namespace))),
      new hydra.core.Field(new hydra.core.Name("local"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).local))))));
  }
  
  static hydra.core.Term termDefinition(hydra.module.TermDefinition x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TermDefinition"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.core.Core.name(((x)).name)),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.core.Core.term(((x)).term)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.typeScheme(((x)).type)))));
  }
  
  static hydra.core.Term typeDefinition(hydra.module.TypeDefinition x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.module.TypeDefinition"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.core.Core.name(((x)).name)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type(((x)).type)))));
  }
}
