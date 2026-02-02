// Note: this is an automatically generated file. Do not edit.

package hydra.encode.typing;

/**
 * Term encoders for hydra.typing
 */
public interface Typing {
  static hydra.core.Term inferenceContext(hydra.typing.InferenceContext x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceContext"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::typeScheme),
        ((x)).schemaTypes))),
      new hydra.core.Field(new hydra.core.Name("primitiveTypes"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::typeScheme),
        ((x)).primitiveTypes))),
      new hydra.core.Field(new hydra.core.Name("dataTypes"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::typeScheme),
        ((x)).dataTypes))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::typeVariableMetadata),
        ((x)).classConstraints))),
      new hydra.core.Field(new hydra.core.Name("debug"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(((x)).debug))))));
  }
  
  static hydra.core.Term inferenceResult(hydra.typing.InferenceResult x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.core.Core.term(((x)).term)),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type(((x)).type)),
      new hydra.core.Field(new hydra.core.Name("subst"), hydra.encode.typing.Typing.typeSubst(((x)).subst)),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::typeVariableMetadata),
        ((x)).classConstraints))))));
  }
  
  static hydra.core.Term termSubst(hydra.typing.TermSubst x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.typing.TermSubst"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
      (hydra.encode.core.Core::name),
      (hydra.encode.core.Core::term),
      ((x)).value))));
  }
  
  static hydra.core.Term typeConstraint(hydra.typing.TypeConstraint x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeConstraint"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("left"), hydra.encode.core.Core.type(((x)).left)),
      new hydra.core.Field(new hydra.core.Name("right"), hydra.encode.core.Core.type(((x)).right)),
      new hydra.core.Field(new hydra.core.Name("comment"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).comment))))));
  }
  
  static hydra.core.Term typeContext(hydra.typing.TypeContext x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeContext"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::type),
        ((x)).types))),
      new hydra.core.Field(new hydra.core.Name("metadata"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        (hydra.encode.core.Core::name),
        (hydra.encode.core.Core::term),
        ((x)).metadata))),
      new hydra.core.Field(new hydra.core.Name("typeVariables"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        (hydra.encode.core.Core::name),
        ((x)).typeVariables))),
      new hydra.core.Field(new hydra.core.Name("lambdaVariables"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        (hydra.encode.core.Core::name),
        ((x)).lambdaVariables))),
      new hydra.core.Field(new hydra.core.Name("letVariables"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        (hydra.encode.core.Core::name),
        ((x)).letVariables))),
      new hydra.core.Field(new hydra.core.Name("inferenceContext"), hydra.encode.typing.Typing.inferenceContext(((x)).inferenceContext)))));
  }
  
  static hydra.core.Term typeSubst(hydra.typing.TypeSubst x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.typing.TypeSubst"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
      (hydra.encode.core.Core::name),
      (hydra.encode.core.Core::type),
      ((x)).value))));
  }
}
