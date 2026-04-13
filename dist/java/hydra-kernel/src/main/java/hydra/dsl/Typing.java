// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.typing
 */
public interface Typing {
  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructure(hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> typeParams, hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> params, hydra.phantoms.TTerm<java.util.List<hydra.core.Binding>> bindings, hydra.phantoms.TTerm<hydra.core.Term> body, hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> domains, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Type>> codomain, hydra.phantoms.TTerm<Env> environment) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), (typeParams).value),
      new hydra.core.Field(new hydra.core.Name("params"), (params).value),
      new hydra.core.Field(new hydra.core.Name("bindings"), (bindings).value),
      new hydra.core.Field(new hydra.core.Name("body"), (body).value),
      new hydra.core.Field(new hydra.core.Name("domains"), (domains).value),
      new hydra.core.Field(new hydra.core.Name("codomain"), (codomain).value),
      new hydra.core.Field(new hydra.core.Name("environment"), (environment).value)))));
  }

  static <Env> hydra.phantoms.TTerm<java.util.List<hydra.core.Binding>> functionStructureBindings(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("bindings"))), (x).value)));
  }

  static <Env> hydra.phantoms.TTerm<hydra.core.Term> functionStructureBody(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("body"))), (x).value)));
  }

  static <Env> hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Type>> functionStructureCodomain(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("codomain"))), (x).value)));
  }

  static <Env> hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> functionStructureDomains(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("domains"))), (x).value)));
  }

  static <Env> hydra.phantoms.TTerm<Env> functionStructureEnvironment(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("environment"))), (x).value)));
  }

  static <Env> hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> functionStructureParams(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("params"))), (x).value)));
  }

  static <Env> hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> functionStructureTypeParams(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("typeParams"))), (x).value)));
  }

  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructureWithBindings(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Binding>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("typeParams"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("params"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("params"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("bindings"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domains"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("domains"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("codomain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("environment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("environment"))), (original).value)))))));
  }

  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructureWithBody(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("typeParams"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("params"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("params"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("domains"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("domains"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("codomain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("environment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("environment"))), (original).value)))))));
  }

  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructureWithCodomain(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("typeParams"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("params"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("params"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domains"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("domains"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("environment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("environment"))), (original).value)))))));
  }

  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructureWithDomains(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("typeParams"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("params"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("params"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domains"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("codomain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("environment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("environment"))), (original).value)))))));
  }

  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructureWithEnvironment(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> original, hydra.phantoms.TTerm<Env> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("typeParams"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("params"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("params"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domains"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("domains"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("codomain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("environment"), (newVal).value)))));
  }

  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructureWithParams(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("typeParams"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("params"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domains"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("domains"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("codomain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("environment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("environment"))), (original).value)))))));
  }

  static <Env> hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> functionStructureWithTypeParams(hydra.phantoms.TTerm<hydra.typing.FunctionStructure<Env>> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.FunctionStructure"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeParams"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("params"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("params"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domains"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("domains"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("codomain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("environment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.FunctionStructure"), new hydra.core.Name("environment"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.typing.InferenceResult> inferenceResult(hydra.phantoms.TTerm<hydra.core.Term> term, hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<hydra.typing.TypeSubst> subst, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> classConstraints, hydra.phantoms.TTerm<hydra.context.Context> context) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("term"), (term).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("subst"), (subst).value),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), (classConstraints).value),
      new hydra.core.Field(new hydra.core.Name("context"), (context).value)))));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> inferenceResultClassConstraints(hydra.phantoms.TTerm<hydra.typing.InferenceResult> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("classConstraints"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.context.Context> inferenceResultContext(hydra.phantoms.TTerm<hydra.typing.InferenceResult> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("context"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeSubst> inferenceResultSubst(hydra.phantoms.TTerm<hydra.typing.InferenceResult> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("subst"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> inferenceResultTerm(hydra.phantoms.TTerm<hydra.typing.InferenceResult> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("term"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> inferenceResultType(hydra.phantoms.TTerm<hydra.typing.InferenceResult> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.typing.InferenceResult> inferenceResultWithClassConstraints(hydra.phantoms.TTerm<hydra.typing.InferenceResult> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("term"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subst"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("subst"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("context"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.typing.InferenceResult> inferenceResultWithContext(hydra.phantoms.TTerm<hydra.typing.InferenceResult> original, hydra.phantoms.TTerm<hydra.context.Context> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("term"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subst"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("subst"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("classConstraints"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.typing.InferenceResult> inferenceResultWithSubst(hydra.phantoms.TTerm<hydra.typing.InferenceResult> original, hydra.phantoms.TTerm<hydra.typing.TypeSubst> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("term"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subst"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("classConstraints"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("context"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.typing.InferenceResult> inferenceResultWithTerm(hydra.phantoms.TTerm<hydra.typing.InferenceResult> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subst"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("subst"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("classConstraints"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("context"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.typing.InferenceResult> inferenceResultWithType(hydra.phantoms.TTerm<hydra.typing.InferenceResult> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.InferenceResult"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("term"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("subst"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("subst"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("classConstraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("classConstraints"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.InferenceResult"), new hydra.core.Name("context"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.typing.TermSubst> termSubst(hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.typing.TermSubst"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeConstraint> typeConstraint(hydra.phantoms.TTerm<hydra.core.Type> left, hydra.phantoms.TTerm<hydra.core.Type> right, hydra.phantoms.TTerm<String> comment) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeConstraint"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (left).value),
      new hydra.core.Field(new hydra.core.Name("right"), (right).value),
      new hydra.core.Field(new hydra.core.Name("comment"), (comment).value)))));
  }

  static hydra.phantoms.TTerm<String> typeConstraintComment(hydra.phantoms.TTerm<hydra.typing.TypeConstraint> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("comment"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeConstraintLeft(hydra.phantoms.TTerm<hydra.typing.TypeConstraint> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("left"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeConstraintRight(hydra.phantoms.TTerm<hydra.typing.TypeConstraint> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("right"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeConstraint> typeConstraintWithComment(hydra.phantoms.TTerm<hydra.typing.TypeConstraint> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeConstraint"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("left"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("right"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("comment"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeConstraint> typeConstraintWithLeft(hydra.phantoms.TTerm<hydra.typing.TypeConstraint> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeConstraint"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("right"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("comment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("comment"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeConstraint> typeConstraintWithRight(hydra.phantoms.TTerm<hydra.typing.TypeConstraint> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.typing.TypeConstraint"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("left"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("comment"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.typing.TypeConstraint"), new hydra.core.Name("comment"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.typing.TypeSubst> typeSubst(hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Type>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.typing.TypeSubst"), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> unTermSubst(hydra.phantoms.TTerm<hydra.typing.TermSubst> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.typing.TermSubst")), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Type>> unTypeSubst(hydra.phantoms.TTerm<hydra.typing.TypeSubst> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.typing.TypeSubst")), (x).value)));
  }
}
