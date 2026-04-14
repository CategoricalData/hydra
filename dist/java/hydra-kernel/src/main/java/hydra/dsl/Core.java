// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.core
 */
public interface Core {
  static hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> annotatedTerm(hydra.phantoms.TTerm<hydra.core.Term> body, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> annotation) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (body).value),
      new hydra.core.Field(new hydra.core.Name("annotation"), (annotation).value)))));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> annotatedTermAnnotation(hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedTerm"), new hydra.core.Name("annotation"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> annotatedTermBody(hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedTerm"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> annotatedTermWithAnnotation(hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedTerm"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("annotation"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> annotatedTermWithBody(hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("annotation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedTerm"), new hydra.core.Name("annotation"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.AnnotatedType> annotatedType(hydra.phantoms.TTerm<hydra.core.Type> body, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> annotation) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (body).value),
      new hydra.core.Field(new hydra.core.Name("annotation"), (annotation).value)))));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> annotatedTypeAnnotation(hydra.phantoms.TTerm<hydra.core.AnnotatedType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedType"), new hydra.core.Name("annotation"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> annotatedTypeBody(hydra.phantoms.TTerm<hydra.core.AnnotatedType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedType"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.AnnotatedType> annotatedTypeWithAnnotation(hydra.phantoms.TTerm<hydra.core.AnnotatedType> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.core.Term>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedType"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("annotation"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.AnnotatedType> annotatedTypeWithBody(hydra.phantoms.TTerm<hydra.core.AnnotatedType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.AnnotatedType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("annotation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.AnnotatedType"), new hydra.core.Name("annotation"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Application> application(hydra.phantoms.TTerm<hydra.core.Term> function, hydra.phantoms.TTerm<hydra.core.Term> argument) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Application"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (function).value),
      new hydra.core.Field(new hydra.core.Name("argument"), (argument).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> applicationArgument(hydra.phantoms.TTerm<hydra.core.Application> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Application"), new hydra.core.Name("argument"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> applicationFunction(hydra.phantoms.TTerm<hydra.core.Application> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Application"), new hydra.core.Name("function"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.ApplicationType> applicationType(hydra.phantoms.TTerm<hydra.core.Type> function, hydra.phantoms.TTerm<hydra.core.Type> argument) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ApplicationType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (function).value),
      new hydra.core.Field(new hydra.core.Name("argument"), (argument).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> applicationTypeArgument(hydra.phantoms.TTerm<hydra.core.ApplicationType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ApplicationType"), new hydra.core.Name("argument"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> applicationTypeFunction(hydra.phantoms.TTerm<hydra.core.ApplicationType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ApplicationType"), new hydra.core.Name("function"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.ApplicationType> applicationTypeWithArgument(hydra.phantoms.TTerm<hydra.core.ApplicationType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ApplicationType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ApplicationType"), new hydra.core.Name("function"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("argument"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.ApplicationType> applicationTypeWithFunction(hydra.phantoms.TTerm<hydra.core.ApplicationType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ApplicationType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("argument"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ApplicationType"), new hydra.core.Name("argument"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Application> applicationWithArgument(hydra.phantoms.TTerm<hydra.core.Application> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Application"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Application"), new hydra.core.Name("function"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("argument"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Application> applicationWithFunction(hydra.phantoms.TTerm<hydra.core.Application> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Application"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("argument"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Application"), new hydra.core.Name("argument"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Binding> binding(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.Term> term, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Binding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("term"), (term).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> bindingName(hydra.phantoms.TTerm<hydra.core.Binding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> bindingTerm(hydra.phantoms.TTerm<hydra.core.Binding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("term"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> bindingType(hydra.phantoms.TTerm<hydra.core.Binding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Binding> bindingWithName(hydra.phantoms.TTerm<hydra.core.Binding> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Binding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("term"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Binding> bindingWithTerm(hydra.phantoms.TTerm<hydra.core.Binding> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Binding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Binding> bindingWithType(hydra.phantoms.TTerm<hydra.core.Binding> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.TypeScheme>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Binding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Binding"), new hydra.core.Name("term"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.CaseStatement> caseStatement(hydra.phantoms.TTerm<hydra.core.Name> typeName, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Term>> default_, hydra.phantoms.TTerm<java.util.List<hydra.core.Field>> cases) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.CaseStatement"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value),
      new hydra.core.Field(new hydra.core.Name("default"), (default_).value),
      new hydra.core.Field(new hydra.core.Name("cases"), (cases).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.core.Field>> caseStatementCases(hydra.phantoms.TTerm<hydra.core.CaseStatement> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("cases"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Term>> caseStatementDefault(hydra.phantoms.TTerm<hydra.core.CaseStatement> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("default"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> caseStatementTypeName(hydra.phantoms.TTerm<hydra.core.CaseStatement> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("typeName"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.CaseStatement> caseStatementWithCases(hydra.phantoms.TTerm<hydra.core.CaseStatement> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Field>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.CaseStatement"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("typeName"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("default"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("default"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.CaseStatement> caseStatementWithDefault(hydra.phantoms.TTerm<hydra.core.CaseStatement> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Term>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.CaseStatement"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("typeName"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("default"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("cases"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.CaseStatement> caseStatementWithTypeName(hydra.phantoms.TTerm<hydra.core.CaseStatement> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.CaseStatement"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("default"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("default"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.CaseStatement"), new hydra.core.Name("cases"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.EitherType> eitherType(hydra.phantoms.TTerm<hydra.core.Type> left, hydra.phantoms.TTerm<hydra.core.Type> right) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.EitherType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (left).value),
      new hydra.core.Field(new hydra.core.Name("right"), (right).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> eitherTypeLeft(hydra.phantoms.TTerm<hydra.core.EitherType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.EitherType"), new hydra.core.Name("left"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> eitherTypeRight(hydra.phantoms.TTerm<hydra.core.EitherType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.EitherType"), new hydra.core.Name("right"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.EitherType> eitherTypeWithLeft(hydra.phantoms.TTerm<hydra.core.EitherType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.EitherType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.EitherType"), new hydra.core.Name("right"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.EitherType> eitherTypeWithRight(hydra.phantoms.TTerm<hydra.core.EitherType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.EitherType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.EitherType"), new hydra.core.Name("left"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Field> field(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.Term> term) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("term"), (term).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> fieldName(hydra.phantoms.TTerm<hydra.core.Field> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> fieldTerm(hydra.phantoms.TTerm<hydra.core.Field> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("term"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.FieldType> fieldType(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FieldType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> fieldTypeName(hydra.phantoms.TTerm<hydra.core.FieldType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FieldType"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> fieldTypeType(hydra.phantoms.TTerm<hydra.core.FieldType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FieldType"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.FieldType> fieldTypeWithName(hydra.phantoms.TTerm<hydra.core.FieldType> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FieldType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FieldType"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.FieldType> fieldTypeWithType(hydra.phantoms.TTerm<hydra.core.FieldType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FieldType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FieldType"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Field> fieldWithName(hydra.phantoms.TTerm<hydra.core.Field> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("term"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Field> fieldWithTerm(hydra.phantoms.TTerm<hydra.core.Field> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Field"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.FloatType> floatTypeBigfloat() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatType"), new hydra.core.Field(new hydra.core.Name("bigfloat"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.FloatType> floatTypeFloat32() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatType"), new hydra.core.Field(new hydra.core.Name("float32"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.FloatType> floatTypeFloat64() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatType"), new hydra.core.Field(new hydra.core.Name("float64"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.FloatValue> floatValueBigfloat(hydra.phantoms.TTerm<java.math.BigDecimal> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatValue"), new hydra.core.Field(new hydra.core.Name("bigfloat"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.FloatValue> floatValueFloat32(hydra.phantoms.TTerm<Float> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatValue"), new hydra.core.Field(new hydra.core.Name("float32"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.FloatValue> floatValueFloat64(hydra.phantoms.TTerm<Double> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.FloatValue"), new hydra.core.Field(new hydra.core.Name("float64"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.ForallType> forallType(hydra.phantoms.TTerm<hydra.core.Name> parameter, hydra.phantoms.TTerm<hydra.core.Type> body) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ForallType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), (parameter).value),
      new hydra.core.Field(new hydra.core.Name("body"), (body).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> forallTypeBody(hydra.phantoms.TTerm<hydra.core.ForallType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ForallType"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> forallTypeParameter(hydra.phantoms.TTerm<hydra.core.ForallType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ForallType"), new hydra.core.Name("parameter"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.ForallType> forallTypeWithBody(hydra.phantoms.TTerm<hydra.core.ForallType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ForallType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ForallType"), new hydra.core.Name("parameter"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.ForallType> forallTypeWithParameter(hydra.phantoms.TTerm<hydra.core.ForallType> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.ForallType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.ForallType"), new hydra.core.Name("body"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.FunctionType> functionType(hydra.phantoms.TTerm<hydra.core.Type> domain, hydra.phantoms.TTerm<hydra.core.Type> codomain) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FunctionType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("domain"), (domain).value),
      new hydra.core.Field(new hydra.core.Name("codomain"), (codomain).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> functionTypeCodomain(hydra.phantoms.TTerm<hydra.core.FunctionType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FunctionType"), new hydra.core.Name("codomain"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> functionTypeDomain(hydra.phantoms.TTerm<hydra.core.FunctionType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FunctionType"), new hydra.core.Name("domain"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.FunctionType> functionTypeWithCodomain(hydra.phantoms.TTerm<hydra.core.FunctionType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FunctionType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FunctionType"), new hydra.core.Name("domain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.FunctionType> functionTypeWithDomain(hydra.phantoms.TTerm<hydra.core.FunctionType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.FunctionType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("domain"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.FunctionType"), new hydra.core.Name("codomain"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Injection> injection(hydra.phantoms.TTerm<hydra.core.Name> typeName, hydra.phantoms.TTerm<hydra.core.Field> field) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Injection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value),
      new hydra.core.Field(new hydra.core.Name("field"), (field).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Field> injectionField(hydra.phantoms.TTerm<hydra.core.Injection> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Injection"), new hydra.core.Name("field"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> injectionTypeName(hydra.phantoms.TTerm<hydra.core.Injection> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Injection"), new hydra.core.Name("typeName"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Injection> injectionWithField(hydra.phantoms.TTerm<hydra.core.Injection> original, hydra.phantoms.TTerm<hydra.core.Field> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Injection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Injection"), new hydra.core.Name("typeName"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("field"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Injection> injectionWithTypeName(hydra.phantoms.TTerm<hydra.core.Injection> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Injection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Injection"), new hydra.core.Name("field"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeBigint() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("bigint"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeInt16() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int16"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeInt32() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int32"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeInt64() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int64"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeInt8() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("int8"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeUint16() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint16"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeUint32() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint32"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeUint64() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint64"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerType> integerTypeUint8() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerType"), new hydra.core.Field(new hydra.core.Name("uint8"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueBigint(hydra.phantoms.TTerm<java.math.BigInteger> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("bigint"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueInt16(hydra.phantoms.TTerm<Short> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int16"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueInt32(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int32"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueInt64(hydra.phantoms.TTerm<Long> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int64"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueInt8(hydra.phantoms.TTerm<Byte> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("int8"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueUint16(hydra.phantoms.TTerm<Character> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint16"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueUint32(hydra.phantoms.TTerm<Long> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint32"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueUint64(hydra.phantoms.TTerm<java.math.BigInteger> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint64"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.IntegerValue> integerValueUint8(hydra.phantoms.TTerm<Short> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.IntegerValue"), new hydra.core.Field(new hydra.core.Name("uint8"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Lambda> lambda(hydra.phantoms.TTerm<hydra.core.Name> parameter, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Type>> domain, hydra.phantoms.TTerm<hydra.core.Term> body) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Lambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), (parameter).value),
      new hydra.core.Field(new hydra.core.Name("domain"), (domain).value),
      new hydra.core.Field(new hydra.core.Name("body"), (body).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> lambdaBody(hydra.phantoms.TTerm<hydra.core.Lambda> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Type>> lambdaDomain(hydra.phantoms.TTerm<hydra.core.Lambda> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("domain"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> lambdaParameter(hydra.phantoms.TTerm<hydra.core.Lambda> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("parameter"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Lambda> lambdaWithBody(hydra.phantoms.TTerm<hydra.core.Lambda> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Lambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("parameter"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("domain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Lambda> lambdaWithDomain(hydra.phantoms.TTerm<hydra.core.Lambda> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Lambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("parameter"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("domain"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("body"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Lambda> lambdaWithParameter(hydra.phantoms.TTerm<hydra.core.Lambda> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Lambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("domain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Lambda"), new hydra.core.Name("body"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Let> let(hydra.phantoms.TTerm<java.util.List<hydra.core.Binding>> bindings, hydra.phantoms.TTerm<hydra.core.Term> body) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Let"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), (bindings).value),
      new hydra.core.Field(new hydra.core.Name("body"), (body).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.core.Binding>> letBindings(hydra.phantoms.TTerm<hydra.core.Let> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Let"), new hydra.core.Name("bindings"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> letBody(hydra.phantoms.TTerm<hydra.core.Let> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Let"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Let> letWithBindings(hydra.phantoms.TTerm<hydra.core.Let> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Binding>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Let"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Let"), new hydra.core.Name("body"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Let> letWithBody(hydra.phantoms.TTerm<hydra.core.Let> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Let"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Let"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Literal> literalBinary(hydra.phantoms.TTerm<byte[]> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("binary"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Literal> literalBoolean(hydra.phantoms.TTerm<Boolean> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("boolean"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Literal> literalFloat(hydra.phantoms.TTerm<hydra.core.FloatValue> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("float"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Literal> literalInteger(hydra.phantoms.TTerm<hydra.core.IntegerValue> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("integer"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Literal> literalString(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Literal"), new hydra.core.Field(new hydra.core.Name("string"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.LiteralType> literalTypeBinary() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("binary"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.LiteralType> literalTypeBoolean() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.LiteralType> literalTypeFloat(hydra.phantoms.TTerm<hydra.core.FloatType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("float"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.LiteralType> literalTypeInteger(hydra.phantoms.TTerm<hydra.core.IntegerType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("integer"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.LiteralType> literalTypeString() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.LiteralType"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.MapType> mapType(hydra.phantoms.TTerm<hydra.core.Type> keys, hydra.phantoms.TTerm<hydra.core.Type> values) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.MapType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keys"), (keys).value),
      new hydra.core.Field(new hydra.core.Name("values"), (values).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> mapTypeKeys(hydra.phantoms.TTerm<hydra.core.MapType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.MapType"), new hydra.core.Name("keys"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> mapTypeValues(hydra.phantoms.TTerm<hydra.core.MapType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.MapType"), new hydra.core.Name("values"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.MapType> mapTypeWithKeys(hydra.phantoms.TTerm<hydra.core.MapType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.MapType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keys"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("values"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.MapType"), new hydra.core.Name("values"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.MapType> mapTypeWithValues(hydra.phantoms.TTerm<hydra.core.MapType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.MapType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keys"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.MapType"), new hydra.core.Name("keys"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("values"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> name(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.core.Name"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.PairType> pairType(hydra.phantoms.TTerm<hydra.core.Type> first, hydra.phantoms.TTerm<hydra.core.Type> second) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.PairType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("first"), (first).value),
      new hydra.core.Field(new hydra.core.Name("second"), (second).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> pairTypeFirst(hydra.phantoms.TTerm<hydra.core.PairType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.PairType"), new hydra.core.Name("first"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> pairTypeSecond(hydra.phantoms.TTerm<hydra.core.PairType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.PairType"), new hydra.core.Name("second"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.PairType> pairTypeWithFirst(hydra.phantoms.TTerm<hydra.core.PairType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.PairType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("first"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("second"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.PairType"), new hydra.core.Name("second"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.PairType> pairTypeWithSecond(hydra.phantoms.TTerm<hydra.core.PairType> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.PairType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("first"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.PairType"), new hydra.core.Name("first"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("second"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Projection> projection(hydra.phantoms.TTerm<hydra.core.Name> typeName, hydra.phantoms.TTerm<hydra.core.Name> field) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Projection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value),
      new hydra.core.Field(new hydra.core.Name("field"), (field).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> projectionField(hydra.phantoms.TTerm<hydra.core.Projection> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Projection"), new hydra.core.Name("field"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> projectionTypeName(hydra.phantoms.TTerm<hydra.core.Projection> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Projection"), new hydra.core.Name("typeName"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Projection> projectionWithField(hydra.phantoms.TTerm<hydra.core.Projection> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Projection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Projection"), new hydra.core.Name("typeName"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("field"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Projection> projectionWithTypeName(hydra.phantoms.TTerm<hydra.core.Projection> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Projection"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Projection"), new hydra.core.Name("field"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Record> record(hydra.phantoms.TTerm<hydra.core.Name> typeName, hydra.phantoms.TTerm<java.util.List<hydra.core.Field>> fields) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value),
      new hydra.core.Field(new hydra.core.Name("fields"), (fields).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.core.Field>> recordFields(hydra.phantoms.TTerm<hydra.core.Record> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Record"), new hydra.core.Name("fields"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> recordTypeName(hydra.phantoms.TTerm<hydra.core.Record> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Record"), new hydra.core.Name("typeName"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Record> recordWithFields(hydra.phantoms.TTerm<hydra.core.Record> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Field>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Record"), new hydra.core.Name("typeName"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fields"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Record> recordWithTypeName(hydra.phantoms.TTerm<hydra.core.Record> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.Record"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.Record"), new hydra.core.Name("fields"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termAnnotated(hydra.phantoms.TTerm<hydra.core.AnnotatedTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("annotated"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termApplication(hydra.phantoms.TTerm<hydra.core.Application> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("application"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termCases(hydra.phantoms.TTerm<hydra.core.CaseStatement> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("cases"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termEither(hydra.phantoms.TTerm<hydra.util.Either<hydra.core.Term, hydra.core.Term>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("either"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termInject(hydra.phantoms.TTerm<hydra.core.Injection> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("inject"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termLambda(hydra.phantoms.TTerm<hydra.core.Lambda> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("lambda"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termLet(hydra.phantoms.TTerm<hydra.core.Let> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("let"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termList(hydra.phantoms.TTerm<java.util.List<hydra.core.Term>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("list"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termLiteral(hydra.phantoms.TTerm<hydra.core.Literal> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("literal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termMap(hydra.phantoms.TTerm<java.util.Map<hydra.core.Term, hydra.core.Term>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("map"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termMaybe(hydra.phantoms.TTerm<hydra.util.Maybe<hydra.core.Term>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("maybe"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termPair(hydra.phantoms.TTerm<hydra.util.Pair<hydra.core.Term, hydra.core.Term>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("pair"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termProject(hydra.phantoms.TTerm<hydra.core.Projection> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("project"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termRecord(hydra.phantoms.TTerm<hydra.core.Record> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("record"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termSet(hydra.phantoms.TTerm<java.util.Set<hydra.core.Term>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("set"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termTypeApplication(hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("typeApplication"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termTypeLambda(hydra.phantoms.TTerm<hydra.core.TypeLambda> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("typeLambda"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termUnit() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termUnwrap(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("unwrap"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termVariable(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("variable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> termWrap(hydra.phantoms.TTerm<hydra.core.WrappedTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Term"), new hydra.core.Field(new hydra.core.Name("wrap"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeAnnotated(hydra.phantoms.TTerm<hydra.core.AnnotatedType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("annotated"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeApplication(hydra.phantoms.TTerm<hydra.core.ApplicationType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("application"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> typeApplicationTerm(hydra.phantoms.TTerm<hydra.core.Term> body, hydra.phantoms.TTerm<hydra.core.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeApplicationTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (body).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> typeApplicationTermBody(hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeApplicationTerm"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeApplicationTermType(hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeApplicationTerm"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> typeApplicationTermWithBody(hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeApplicationTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeApplicationTerm"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> typeApplicationTermWithType(hydra.phantoms.TTerm<hydra.core.TypeApplicationTerm> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeApplicationTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeApplicationTerm"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeEither(hydra.phantoms.TTerm<hydra.core.EitherType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("either"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeForall(hydra.phantoms.TTerm<hydra.core.ForallType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("forall"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeFunction(hydra.phantoms.TTerm<hydra.core.FunctionType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("function"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeLambda> typeLambda(hydra.phantoms.TTerm<hydra.core.Name> parameter, hydra.phantoms.TTerm<hydra.core.Term> body) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeLambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), (parameter).value),
      new hydra.core.Field(new hydra.core.Name("body"), (body).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> typeLambdaBody(hydra.phantoms.TTerm<hydra.core.TypeLambda> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeLambda"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> typeLambdaParameter(hydra.phantoms.TTerm<hydra.core.TypeLambda> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeLambda"), new hydra.core.Name("parameter"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeLambda> typeLambdaWithBody(hydra.phantoms.TTerm<hydra.core.TypeLambda> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeLambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeLambda"), new hydra.core.Name("parameter"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeLambda> typeLambdaWithParameter(hydra.phantoms.TTerm<hydra.core.TypeLambda> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeLambda"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("parameter"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeLambda"), new hydra.core.Name("body"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeList(hydra.phantoms.TTerm<hydra.core.Type> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("list"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeLiteral(hydra.phantoms.TTerm<hydra.core.LiteralType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("literal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeMap(hydra.phantoms.TTerm<hydra.core.MapType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("map"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeMaybe(hydra.phantoms.TTerm<hydra.core.Type> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("maybe"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typePair(hydra.phantoms.TTerm<hydra.core.PairType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("pair"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeRecord(hydra.phantoms.TTerm<java.util.List<hydra.core.FieldType>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("record"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeScheme> typeScheme(hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> variables, hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> constraints) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeScheme"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), (variables).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("constraints"), (constraints).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> typeSchemeConstraints(hydra.phantoms.TTerm<hydra.core.TypeScheme> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("constraints"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeSchemeType(hydra.phantoms.TTerm<hydra.core.TypeScheme> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> typeSchemeVariables(hydra.phantoms.TTerm<hydra.core.TypeScheme> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("variables"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeScheme> typeSchemeWithConstraints(hydra.phantoms.TTerm<hydra.core.TypeScheme> original, hydra.phantoms.TTerm<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeScheme"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("variables"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("constraints"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeScheme> typeSchemeWithType(hydra.phantoms.TTerm<hydra.core.TypeScheme> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeScheme"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("variables"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("constraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("constraints"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeScheme> typeSchemeWithVariables(hydra.phantoms.TTerm<hydra.core.TypeScheme> original, hydra.phantoms.TTerm<java.util.List<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeScheme"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("variables"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("type"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("constraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeScheme"), new hydra.core.Name("constraints"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeSet(hydra.phantoms.TTerm<hydra.core.Type> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("set"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeUnion(hydra.phantoms.TTerm<java.util.List<hydra.core.FieldType>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("union"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeUnit() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("unit"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeVariable(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("variable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeVariableMetadata> typeVariableMetadata(hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> classes) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeVariableMetadata"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("classes"), (classes).value)))));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> typeVariableMetadataClasses(hydra.phantoms.TTerm<hydra.core.TypeVariableMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.TypeVariableMetadata"), new hydra.core.Name("classes"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeVariableMetadata> typeVariableMetadataWithClasses(hydra.phantoms.TTerm<hydra.core.TypeVariableMetadata> original, hydra.phantoms.TTerm<java.util.Set<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.TypeVariableMetadata"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("classes"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeVoid() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("void"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeWrap(hydra.phantoms.TTerm<hydra.core.Type> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.core.Type"), new hydra.core.Field(new hydra.core.Name("wrap"), (x).value))));
  }

  static hydra.phantoms.TTerm<String> unName(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.core.Name")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.WrappedTerm> wrappedTerm(hydra.phantoms.TTerm<hydra.core.Name> typeName, hydra.phantoms.TTerm<hydra.core.Term> body) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (typeName).value),
      new hydra.core.Field(new hydra.core.Name("body"), (body).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> wrappedTermBody(hydra.phantoms.TTerm<hydra.core.WrappedTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.WrappedTerm"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> wrappedTermTypeName(hydra.phantoms.TTerm<hydra.core.WrappedTerm> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.WrappedTerm"), new hydra.core.Name("typeName"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.WrappedTerm> wrappedTermWithBody(hydra.phantoms.TTerm<hydra.core.WrappedTerm> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.WrappedTerm"), new hydra.core.Name("typeName"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.WrappedTerm> wrappedTermWithTypeName(hydra.phantoms.TTerm<hydra.core.WrappedTerm> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.core.WrappedTerm"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.core.WrappedTerm"), new hydra.core.Name("body"))), (original).value)))))));
  }
}
