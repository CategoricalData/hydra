// Note: this is an automatically generated file. Do not edit.

package hydra.encode.typing;

/**
 * Term encoders for hydra.typing
 */
public interface Typing {
  static <T0> hydra.core.Term functionStructure(java.util.function.Function<T0, hydra.core.Term> env, hydra.typing.FunctionStructure<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("typeParams"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.core.Core::name,
        ((java.util.function.Function<hydra.typing.FunctionStructure<T0>, java.util.List<hydra.core.Name>>) (projected -> projected.typeParams)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("params"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.core.Core::name,
        ((java.util.function.Function<hydra.typing.FunctionStructure<T0>, java.util.List<hydra.core.Name>>) (projected -> projected.params)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.core.Core::binding,
        ((java.util.function.Function<hydra.typing.FunctionStructure<T0>, java.util.List<hydra.core.Binding>>) (projected -> projected.bindings)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("body"), hydra.encode.core.Core.term(((java.util.function.Function<hydra.typing.FunctionStructure<T0>, hydra.core.Term>) (projected -> projected.body)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("domains"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.core.Core::type,
        ((java.util.function.Function<hydra.typing.FunctionStructure<T0>, java.util.List<hydra.core.Type>>) (projected -> projected.domains)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        hydra.encode.core.Core::type,
        ((java.util.function.Function<hydra.typing.FunctionStructure<T0>, hydra.util.Maybe<hydra.core.Type>>) (projected -> projected.codomain)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("environment"), (env).apply(((java.util.function.Function<hydra.typing.FunctionStructure<T0>, T0>) (projected -> projected.environment)).apply(x))))));
  }
  
  static hydra.core.Term inferenceContext(hydra.typing.InferenceContext x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceContext"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::typeScheme,
        (x).schemaTypes))),
      new hydra.core.Field(new hydra.core.Name("primitiveTypes"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::typeScheme,
        (x).primitiveTypes))),
      new hydra.core.Field(new hydra.core.Name("dataTypes"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::typeScheme,
        (x).dataTypes))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::typeVariableMetadata,
        (x).classConstraints))),
      new hydra.core.Field(new hydra.core.Name("debug"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_((x).debug))))));
  }
  
  static hydra.core.Term inferenceResult(hydra.typing.InferenceResult x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.core.Core.term((x).term)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type((x).type)),
      new hydra.core.Field(new hydra.core.Name("subst"), hydra.encode.typing.Typing.typeSubst((x).subst)),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::typeVariableMetadata,
        (x).classConstraints))))));
  }
  
  static hydra.core.Term termSubst(hydra.typing.TermSubst x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.typing.TermSubst"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
      hydra.encode.core.Core::name,
      hydra.encode.core.Core::term,
      (x).value))));
  }
  
  static hydra.core.Term typeConstraint(hydra.typing.TypeConstraint x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeConstraint"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("left"), hydra.encode.core.Core.type((x).left)),
      new hydra.core.Field(new hydra.core.Name("right"), hydra.encode.core.Core.type((x).right)),
      new hydra.core.Field(new hydra.core.Name("comment"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).comment))))));
  }
  
  static hydra.core.Term typeContext(hydra.typing.TypeContext x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeContext"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::type,
        (x).types))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.core.Core::name,
        hydra.encode.core.Core::term,
        (x).metadata))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        hydra.encode.core.Core::name,
        (x).typeVariables))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        hydra.encode.core.Core::name,
        (x).lambdaVariables))),
      new hydra.core.Field(new hydra.core.Name("letVariables"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        hydra.encode.core.Core::name,
        (x).letVariables))),
      new hydra.core.Field(new hydra.core.Name("inferenceContext"), hydra.encode.typing.Typing.inferenceContext((x).inferenceContext)))));
  }
  
  static hydra.core.Term typeSubst(hydra.typing.TypeSubst x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.typing.TypeSubst"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
      hydra.encode.core.Core::name,
      hydra.encode.core.Core::type,
      (x).value))));
  }
}
