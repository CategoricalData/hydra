// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.haskell;

/**
 * DSL functions for hydra.haskell.syntax
 */
public interface Syntax {
  static hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> alternative(hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> pattern, hydra.phantoms.TTerm<hydra.haskell.syntax.CaseRhs> rhs, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>> binds) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Alternative"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), (pattern).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), (rhs).value),
      new hydra.core.Field(new hydra.core.Name("binds"), (binds).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>> alternativeBinds(hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("binds"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> alternativePattern(hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("pattern"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.CaseRhs> alternativeRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("rhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> alternativeWithBinds(hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Alternative"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("pattern"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("rhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("binds"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> alternativeWithPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Alternative"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("rhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("binds"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("binds"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> alternativeWithRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.Alternative> original, hydra.phantoms.TTerm<hydra.haskell.syntax.CaseRhs> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Alternative"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("pattern"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("binds"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Alternative"), new hydra.core.Name("binds"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> applicationDeclarationHead(hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> function, hydra.phantoms.TTerm<hydra.haskell.syntax.Variable> operand) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (function).value),
      new hydra.core.Field(new hydra.core.Name("operand"), (operand).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> applicationDeclarationHeadFunction(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), new hydra.core.Name("function"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Variable> applicationDeclarationHeadOperand(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), new hydra.core.Name("operand"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> applicationDeclarationHeadWithFunction(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> original, hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("operand"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), new hydra.core.Name("operand"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> applicationDeclarationHeadWithOperand(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Variable> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationDeclarationHead"), new hydra.core.Name("function"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operand"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> applicationExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> function, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> argument) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (function).value),
      new hydra.core.Field(new hydra.core.Name("argument"), (argument).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> applicationExpressionArgument(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), new hydra.core.Name("argument"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> applicationExpressionFunction(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), new hydra.core.Name("function"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> applicationExpressionWithArgument(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), new hydra.core.Name("function"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("argument"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> applicationExpressionWithFunction(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("function"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("argument"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression"), new hydra.core.Name("argument"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> applicationPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> args) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("args"), (args).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> applicationPatternArgs(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), new hydra.core.Name("args"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> applicationPatternName(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> applicationPatternWithArgs(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("args"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> applicationPatternWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("args"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationPattern"), new hydra.core.Name("args"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> applicationType(hydra.phantoms.TTerm<hydra.haskell.syntax.Type> context, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> argument) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("context"), (context).value),
      new hydra.core.Field(new hydra.core.Name("argument"), (argument).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> applicationTypeArgument(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationType"), new hydra.core.Name("argument"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> applicationTypeContext(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationType"), new hydra.core.Name("context"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> applicationTypeWithArgument(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationType"), new hydra.core.Name("context"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("argument"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> applicationTypeWithContext(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ApplicationType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("context"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("argument"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ApplicationType"), new hydra.core.Name("argument"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> asPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> inner) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.AsPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("inner"), (inner).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> asPatternInner(hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.AsPattern"), new hydra.core.Name("inner"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> asPatternName(hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.AsPattern"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> asPatternWithInner(hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.AsPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.AsPattern"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("inner"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> asPatternWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.AsPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("inner"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.AsPattern"), new hydra.core.Name("inner"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Assertion> assertionClass(hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Assertion"), new hydra.core.Field(new hydra.core.Name("class"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Assertion> assertionTuple(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Assertion>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Assertion"), new hydra.core.Field(new hydra.core.Name("tuple"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> caseExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> case_, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Alternative>> alternatives) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.CaseExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("case"), (case_).value),
      new hydra.core.Field(new hydra.core.Name("alternatives"), (alternatives).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Alternative>> caseExpressionAlternatives(hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.CaseExpression"), new hydra.core.Name("alternatives"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> caseExpressionCase(hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.CaseExpression"), new hydra.core.Name("case"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> caseExpressionWithAlternatives(hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Alternative>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.CaseExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("case"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.CaseExpression"), new hydra.core.Name("case"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("alternatives"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> caseExpressionWithCase(hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.CaseExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("case"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("alternatives"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.CaseExpression"), new hydra.core.Name("alternatives"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.CaseRhs> caseRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.CaseRhs"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> classAssertion(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Type>> types) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("types"), (types).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> classAssertionName(hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Type>> classAssertionTypes(hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), new hydra.core.Name("types"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> classAssertionWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> classAssertionWithTypes(hydra.phantoms.TTerm<hydra.haskell.syntax.ClassAssertion> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ClassAssertion"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> constructRecordExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldUpdate>> fields) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("fields"), (fields).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldUpdate>> constructRecordExpressionFields(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), new hydra.core.Name("fields"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> constructRecordExpressionName(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> constructRecordExpressionWithFields(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldUpdate>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fields"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> constructRecordExpressionWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructRecordExpression"), new hydra.core.Name("fields"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Constructor> constructorOrdinary(hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Constructor"), new hydra.core.Field(new hydra.core.Name("ordinary"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Constructor> constructorRecord(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Constructor"), new hydra.core.Field(new hydra.core.Name("record"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructorWithComments> constructorWithComments(hydra.phantoms.TTerm<hydra.haskell.syntax.Constructor> body, hydra.phantoms.TTerm<hydra.util.Maybe<String>> comments) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (body).value),
      new hydra.core.Field(new hydra.core.Name("comments"), (comments).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Constructor> constructorWithCommentsBody(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructorWithComments> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> constructorWithCommentsComments(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructorWithComments> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), new hydra.core.Name("comments"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructorWithComments> constructorWithCommentsWithBody(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructorWithComments> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Constructor> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("comments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), new hydra.core.Name("comments"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructorWithComments> constructorWithCommentsWithComments(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructorWithComments> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ConstructorWithComments"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("comments"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> contextType(hydra.phantoms.TTerm<hydra.haskell.syntax.Assertion> ctx, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ContextType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("ctx"), (ctx).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Assertion> contextTypeCtx(hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ContextType"), new hydra.core.Name("ctx"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> contextTypeType(hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ContextType"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> contextTypeWithCtx(hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Assertion> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ContextType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("ctx"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ContextType"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> contextTypeWithType(hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ContextType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("ctx"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ContextType"), new hydra.core.Name("ctx"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> dataDeclaration(hydra.phantoms.TTerm<hydra.haskell.syntax.DataOrNewtype> keyword, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Assertion>> context, hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> head, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.ConstructorWithComments>> constructors, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Deriving>> deriving) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keyword"), (keyword).value),
      new hydra.core.Field(new hydra.core.Name("context"), (context).value),
      new hydra.core.Field(new hydra.core.Name("head"), (head).value),
      new hydra.core.Field(new hydra.core.Name("constructors"), (constructors).value),
      new hydra.core.Field(new hydra.core.Name("deriving"), (deriving).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.ConstructorWithComments>> dataDeclarationConstructors(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("constructors"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Assertion>> dataDeclarationContext(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("context"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Deriving>> dataDeclarationDeriving(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("deriving"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> dataDeclarationHead(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("head"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataOrNewtype> dataDeclarationKeyword(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("keyword"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> dataDeclarationWithConstructors(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.ConstructorWithComments>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keyword"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("keyword"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("context"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("head"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("constructors"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("deriving"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("deriving"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> dataDeclarationWithContext(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Assertion>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keyword"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("keyword"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("head"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("constructors"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("constructors"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("deriving"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("deriving"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> dataDeclarationWithDeriving(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Deriving>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keyword"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("keyword"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("context"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("head"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("constructors"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("constructors"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("deriving"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> dataDeclarationWithHead(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> original, hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keyword"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("keyword"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("context"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("head"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("constructors"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("constructors"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("deriving"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("deriving"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> dataDeclarationWithKeyword(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> original, hydra.phantoms.TTerm<hydra.haskell.syntax.DataOrNewtype> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("keyword"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("context"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("context"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("head"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("constructors"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("constructors"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("deriving"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DataDeclaration"), new hydra.core.Name("deriving"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataOrNewtype> dataOrNewtypeData() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.DataOrNewtype"), new hydra.core.Field(new hydra.core.Name("data"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DataOrNewtype> dataOrNewtypeNewtype() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.DataOrNewtype"), new hydra.core.Field(new hydra.core.Name("newtype"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Declaration> declarationData(hydra.phantoms.TTerm<hydra.haskell.syntax.DataDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Declaration"), new hydra.core.Field(new hydra.core.Name("data"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> declarationHeadApplication(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationDeclarationHead> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.DeclarationHead"), new hydra.core.Field(new hydra.core.Name("application"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> declarationHeadParens(hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.DeclarationHead"), new hydra.core.Field(new hydra.core.Name("parens"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> declarationHeadSimple(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.DeclarationHead"), new hydra.core.Field(new hydra.core.Name("simple"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Declaration> declarationType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Declaration"), new hydra.core.Field(new hydra.core.Name("type"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Declaration> declarationTypedBinding(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Declaration"), new hydra.core.Field(new hydra.core.Name("typedBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Declaration> declarationValueBinding(hydra.phantoms.TTerm<hydra.haskell.syntax.ValueBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Declaration"), new hydra.core.Field(new hydra.core.Name("valueBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationWithComments> declarationWithComments(hydra.phantoms.TTerm<hydra.haskell.syntax.Declaration> body, hydra.phantoms.TTerm<hydra.util.Maybe<String>> comments) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (body).value),
      new hydra.core.Field(new hydra.core.Name("comments"), (comments).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Declaration> declarationWithCommentsBody(hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationWithComments> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), new hydra.core.Name("body"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> declarationWithCommentsComments(hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationWithComments> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), new hydra.core.Name("comments"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationWithComments> declarationWithCommentsWithBody(hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationWithComments> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Declaration> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("comments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), new hydra.core.Name("comments"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationWithComments> declarationWithCommentsWithComments(hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationWithComments> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("body"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.DeclarationWithComments"), new hydra.core.Name("body"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("comments"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Deriving> deriving(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Name>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.Deriving"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Export> exportDeclaration(hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Export"), new hydra.core.Field(new hydra.core.Name("declaration"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Export> exportModule(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Export"), new hydra.core.Field(new hydra.core.Name("module"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionApplication(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("application"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionCase(hydra.phantoms.TTerm<hydra.haskell.syntax.CaseExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("case"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionConstructRecord(hydra.phantoms.TTerm<hydra.haskell.syntax.ConstructRecordExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("constructRecord"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionDo(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Statement>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("do"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionIf(hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("if"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionInfixApplication(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("infixApplication"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionLambda(hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("lambda"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionLeftSection(hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("leftSection"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionLet(hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("let"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionList(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Expression>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("list"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionLiteral(hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("literal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionParens(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("parens"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionPrefixApplication(hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("prefixApplication"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionRightSection(hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("rightSection"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionTuple(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Expression>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("tuple"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionTypeSignature(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("typeSignature"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionUpdateRecord(hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("updateRecord"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expressionVariable(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Expression"), new hydra.core.Field(new hydra.core.Name("variable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Field> field(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> fieldName(hydra.phantoms.TTerm<hydra.haskell.syntax.Field> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Field"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> fieldType(hydra.phantoms.TTerm<hydra.haskell.syntax.Field> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Field"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FieldUpdate> fieldUpdate(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> value) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("value"), (value).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> fieldUpdateName(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldUpdate> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> fieldUpdateValue(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldUpdate> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), new hydra.core.Name("value"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FieldUpdate> fieldUpdateWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldUpdate> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), new hydra.core.Name("value"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FieldUpdate> fieldUpdateWithValue(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldUpdate> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldUpdate"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("value"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FieldWithComments> fieldWithComments(hydra.phantoms.TTerm<hydra.haskell.syntax.Field> field, hydra.phantoms.TTerm<hydra.util.Maybe<String>> comments) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("field"), (field).value),
      new hydra.core.Field(new hydra.core.Name("comments"), (comments).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> fieldWithCommentsComments(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldWithComments> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), new hydra.core.Name("comments"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Field> fieldWithCommentsField(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldWithComments> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), new hydra.core.Name("field"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FieldWithComments> fieldWithCommentsWithComments(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldWithComments> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("field"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), new hydra.core.Name("field"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("comments"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FieldWithComments> fieldWithCommentsWithField(hydra.phantoms.TTerm<hydra.haskell.syntax.FieldWithComments> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Field> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("field"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("comments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FieldWithComments"), new hydra.core.Name("comments"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Field> fieldWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.Field> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Field"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Field> fieldWithType(hydra.phantoms.TTerm<hydra.haskell.syntax.Field> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Field"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Field"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> functionType(hydra.phantoms.TTerm<hydra.haskell.syntax.Type> domain, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> codomain) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FunctionType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("domain"), (domain).value),
      new hydra.core.Field(new hydra.core.Name("codomain"), (codomain).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> functionTypeCodomain(hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FunctionType"), new hydra.core.Name("codomain"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> functionTypeDomain(hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FunctionType"), new hydra.core.Name("domain"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> functionTypeWithCodomain(hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FunctionType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FunctionType"), new hydra.core.Name("domain"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("codomain"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> functionTypeWithDomain(hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.FunctionType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("domain"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("codomain"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.FunctionType"), new hydra.core.Name("codomain"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> ifExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> condition, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> then, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> else_) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("condition"), (condition).value),
      new hydra.core.Field(new hydra.core.Name("then"), (then).value),
      new hydra.core.Field(new hydra.core.Name("else"), (else_).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> ifExpressionCondition(hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("condition"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> ifExpressionElse(hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("else"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> ifExpressionThen(hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("then"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> ifExpressionWithCondition(hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("condition"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("then"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("then"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("else"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("else"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> ifExpressionWithElse(hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("condition"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("condition"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("then"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("then"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("else"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> ifExpressionWithThen(hydra.phantoms.TTerm<hydra.haskell.syntax.IfExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("condition"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("condition"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("then"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("else"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.IfExpression"), new hydra.core.Name("else"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Import> import_(hydra.phantoms.TTerm<Boolean> qualified, hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> module, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ModuleName>> as, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.SpecImport>> spec) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Import"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualified"), (qualified).value),
      new hydra.core.Field(new hydra.core.Name("module"), (module).value),
      new hydra.core.Field(new hydra.core.Name("as"), (as).value),
      new hydra.core.Field(new hydra.core.Name("spec"), (spec).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ModuleName>> importAs(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("as"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> importExportSpec(hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ImportModifier>> modifier, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.SubspecImportExportSpec>> subspec) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("modifier"), (modifier).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("subspec"), (subspec).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ImportModifier>> importExportSpecModifier(hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("modifier"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> importExportSpecName(hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.SubspecImportExportSpec>> importExportSpecSubspec(hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("subspec"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> importExportSpecWithModifier(hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ImportModifier>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("modifier"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subspec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("subspec"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> importExportSpecWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("modifier"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("modifier"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("subspec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("subspec"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> importExportSpecWithSubspec(hydra.phantoms.TTerm<hydra.haskell.syntax.ImportExportSpec> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.SubspecImportExportSpec>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("modifier"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("modifier"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ImportExportSpec"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subspec"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ImportModifier> importModifierPattern() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.ImportModifier"), new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ImportModifier> importModifierType() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.ImportModifier"), new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> importModule(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("module"))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> importQualified(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("qualified"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.SpecImport>> importSpec(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("spec"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Import> importWithAs(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ModuleName>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Import"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualified"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("qualified"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("module"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("module"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("as"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("spec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("spec"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Import> importWithModule(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> original, hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Import"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualified"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("qualified"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("module"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("as"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("as"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("spec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("spec"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Import> importWithQualified(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Import"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualified"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("module"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("module"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("as"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("as"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("spec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("spec"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Import> importWithSpec(hydra.phantoms.TTerm<hydra.haskell.syntax.Import> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.SpecImport>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Import"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualified"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("qualified"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("module"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("module"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("as"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Import"), new hydra.core.Name("as"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("spec"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> infixApplicationExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> lhs, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> operator, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> rhs) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), (lhs).value),
      new hydra.core.Field(new hydra.core.Name("operator"), (operator).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), (rhs).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> infixApplicationExpressionLhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("lhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> infixApplicationExpressionOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("operator"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> infixApplicationExpressionRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("rhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> infixApplicationExpressionWithLhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("operator"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("operator"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("rhs"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> infixApplicationExpressionWithOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("lhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operator"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("rhs"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> infixApplicationExpressionWithRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixApplicationExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("lhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operator"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixApplicationExpression"), new hydra.core.Name("operator"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> infixType(hydra.phantoms.TTerm<hydra.haskell.syntax.Type> lhs, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> operator, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> rhs) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), (lhs).value),
      new hydra.core.Field(new hydra.core.Name("operator"), (operator).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), (rhs).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> infixTypeLhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("lhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> infixTypeOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("operator"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> infixTypeRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("rhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> infixTypeWithLhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("operator"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("operator"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("rhs"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> infixTypeWithOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("lhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operator"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("rhs"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> infixTypeWithRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.InfixType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("lhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operator"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.InfixType"), new hydra.core.Name("operator"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> lambdaExpression(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> bindings, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> inner) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), (bindings).value),
      new hydra.core.Field(new hydra.core.Name("inner"), (inner).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> lambdaExpressionBindings(hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), new hydra.core.Name("bindings"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> lambdaExpressionInner(hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), new hydra.core.Name("inner"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> lambdaExpressionWithBindings(hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("inner"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), new hydra.core.Name("inner"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> lambdaExpressionWithInner(hydra.phantoms.TTerm<hydra.haskell.syntax.LambdaExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LambdaExpression"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("inner"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> letExpression(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.LocalBinding>> bindings, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> inner) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.LetExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), (bindings).value),
      new hydra.core.Field(new hydra.core.Name("inner"), (inner).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.LocalBinding>> letExpressionBindings(hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LetExpression"), new hydra.core.Name("bindings"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> letExpressionInner(hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LetExpression"), new hydra.core.Name("inner"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> letExpressionWithBindings(hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.LocalBinding>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.LetExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("inner"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LetExpression"), new hydra.core.Name("inner"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> letExpressionWithInner(hydra.phantoms.TTerm<hydra.haskell.syntax.LetExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.LetExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.LetExpression"), new hydra.core.Name("bindings"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("inner"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> literalChar(hydra.phantoms.TTerm<Character> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Literal"), new hydra.core.Field(new hydra.core.Name("char"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> literalDouble(hydra.phantoms.TTerm<Double> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Literal"), new hydra.core.Field(new hydra.core.Name("double"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> literalFloat(hydra.phantoms.TTerm<Float> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Literal"), new hydra.core.Field(new hydra.core.Name("float"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> literalInt(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Literal"), new hydra.core.Field(new hydra.core.Name("int"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> literalInteger(hydra.phantoms.TTerm<java.math.BigInteger> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Literal"), new hydra.core.Field(new hydra.core.Name("integer"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> literalString(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Literal"), new hydra.core.Field(new hydra.core.Name("string"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LocalBinding> localBindingSignature(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.LocalBinding"), new hydra.core.Field(new hydra.core.Name("signature"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LocalBinding> localBindingValue(hydra.phantoms.TTerm<hydra.haskell.syntax.ValueBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.LocalBinding"), new hydra.core.Field(new hydra.core.Name("value"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.LocalBindings> localBindings(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.LocalBinding>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.LocalBindings"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Module> module(hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ModuleHead>> head, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Import>> imports, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.DeclarationWithComments>> declarations) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Module"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("head"), (head).value),
      new hydra.core.Field(new hydra.core.Name("imports"), (imports).value),
      new hydra.core.Field(new hydra.core.Name("declarations"), (declarations).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.DeclarationWithComments>> moduleDeclarations(hydra.phantoms.TTerm<hydra.haskell.syntax.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("declarations"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ModuleHead>> moduleHead(hydra.phantoms.TTerm<hydra.haskell.syntax.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("head"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> moduleHeadComments(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("comments"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Export>> moduleHeadExports(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("exports"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> moduleHeadName(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> moduleHeadWithComments(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("comments"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("exports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("exports"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> moduleHeadWithExports(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Export>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("comments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("comments"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("exports"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> moduleHeadWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> original, hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("comments"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("comments"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("exports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), new hydra.core.Name("exports"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleHead> moduleHead_(hydra.phantoms.TTerm<hydra.util.Maybe<String>> comments, hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> name, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Export>> exports) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.ModuleHead"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("comments"), (comments).value),
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("exports"), (exports).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Import>> moduleImports(hydra.phantoms.TTerm<hydra.haskell.syntax.Module> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("imports"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> moduleName(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.ModuleName"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Module> moduleWithDeclarations(hydra.phantoms.TTerm<hydra.haskell.syntax.Module> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.DeclarationWithComments>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Module"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("head"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("imports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("imports"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("declarations"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Module> moduleWithHead(hydra.phantoms.TTerm<hydra.haskell.syntax.Module> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.ModuleHead>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Module"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("head"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("imports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("imports"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("declarations"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("declarations"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Module> moduleWithImports(hydra.phantoms.TTerm<hydra.haskell.syntax.Module> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Import>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.Module"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("head"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("head"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("imports"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("declarations"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.Module"), new hydra.core.Name("declarations"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> nameImplicit(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Name"), new hydra.core.Field(new hydra.core.Name("implicit"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> nameNormal(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Name"), new hydra.core.Field(new hydra.core.Name("normal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> nameParens(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Name"), new hydra.core.Field(new hydra.core.Name("parens"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.NamePart> namePart(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.NamePart"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> operatorBacktick(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Operator"), new hydra.core.Field(new hydra.core.Name("backtick"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> operatorNormal(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Operator"), new hydra.core.Field(new hydra.core.Name("normal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> ordinaryConstructor(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Type>> fields) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("fields"), (fields).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Type>> ordinaryConstructorFields(hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), new hydra.core.Name("fields"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> ordinaryConstructorName(hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> ordinaryConstructorWithFields(hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Type>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fields"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> ordinaryConstructorWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.OrdinaryConstructor> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.OrdinaryConstructor"), new hydra.core.Name("fields"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternApplication(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("application"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternAs(hydra.phantoms.TTerm<hydra.haskell.syntax.AsPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("as"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.PatternField> patternField(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> pattern) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.PatternField"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("pattern"), (pattern).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> patternFieldName(hydra.phantoms.TTerm<hydra.haskell.syntax.PatternField> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PatternField"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternFieldPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.PatternField> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PatternField"), new hydra.core.Name("pattern"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.PatternField> patternFieldWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.PatternField> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.PatternField"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PatternField"), new hydra.core.Name("pattern"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.PatternField> patternFieldWithPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.PatternField> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.PatternField"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PatternField"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("pattern"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternList(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("list"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternLiteral(hydra.phantoms.TTerm<hydra.haskell.syntax.Literal> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("literal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternName(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("name"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternParens(hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("parens"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternRecord(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("record"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternTuple(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Pattern>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("tuple"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternTyped(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("typed"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> patternWildcard() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Pattern"), new hydra.core.Field(new hydra.core.Name("wildcard"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> prefixApplicationExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> operator, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> rhs) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("operator"), (operator).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), (rhs).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> prefixApplicationExpressionOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), new hydra.core.Name("operator"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> prefixApplicationExpressionRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), new hydra.core.Name("rhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> prefixApplicationExpressionWithOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("operator"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), new hydra.core.Name("rhs"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> prefixApplicationExpressionWithRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.PrefixApplicationExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("operator"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.PrefixApplicationExpression"), new hydra.core.Name("operator"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> qualifiedName(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.NamePart>> qualifiers, hydra.phantoms.TTerm<hydra.haskell.syntax.NamePart> unqualified) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.QualifiedName"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualifiers"), (qualifiers).value),
      new hydra.core.Field(new hydra.core.Name("unqualified"), (unqualified).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.NamePart>> qualifiedNameQualifiers(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.QualifiedName"), new hydra.core.Name("qualifiers"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.NamePart> qualifiedNameUnqualified(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.QualifiedName"), new hydra.core.Name("unqualified"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> qualifiedNameWithQualifiers(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.NamePart>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.QualifiedName"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualifiers"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("unqualified"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.QualifiedName"), new hydra.core.Name("unqualified"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> qualifiedNameWithUnqualified(hydra.phantoms.TTerm<hydra.haskell.syntax.QualifiedName> original, hydra.phantoms.TTerm<hydra.haskell.syntax.NamePart> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.QualifiedName"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("qualifiers"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.QualifiedName"), new hydra.core.Name("qualifiers"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("unqualified"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> recordConstructor(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldWithComments>> fields) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("fields"), (fields).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldWithComments>> recordConstructorFields(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), new hydra.core.Name("fields"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> recordConstructorName(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> recordConstructorWithFields(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldWithComments>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fields"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> recordConstructorWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordConstructor> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordConstructor"), new hydra.core.Name("fields"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> recordPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.PatternField>> fields) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.RecordPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("fields"), (fields).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.PatternField>> recordPatternFields(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordPattern"), new hydra.core.Name("fields"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> recordPatternName(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordPattern"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> recordPatternWithFields(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.PatternField>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.RecordPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordPattern"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fields"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> recordPatternWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.RecordPattern> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.RecordPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.RecordPattern"), new hydra.core.Name("fields"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RightHandSide> rightHandSide(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.RightHandSide"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> sectionExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> operator, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> expression) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.SectionExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("operator"), (operator).value),
      new hydra.core.Field(new hydra.core.Name("expression"), (expression).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> sectionExpressionExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SectionExpression"), new hydra.core.Name("expression"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> sectionExpressionOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SectionExpression"), new hydra.core.Name("operator"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> sectionExpressionWithExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.SectionExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("operator"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SectionExpression"), new hydra.core.Name("operator"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expression"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> sectionExpressionWithOperator(hydra.phantoms.TTerm<hydra.haskell.syntax.SectionExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Operator> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.SectionExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("operator"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expression"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SectionExpression"), new hydra.core.Name("expression"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> simpleValueBinding(hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> pattern, hydra.phantoms.TTerm<hydra.haskell.syntax.RightHandSide> rhs, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>> localBindings) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), (pattern).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), (rhs).value),
      new hydra.core.Field(new hydra.core.Name("localBindings"), (localBindings).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>> simpleValueBindingLocalBindings(hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("localBindings"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> simpleValueBindingPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("pattern"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.RightHandSide> simpleValueBindingRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("rhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> simpleValueBindingWithLocalBindings(hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.haskell.syntax.LocalBindings>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("pattern"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("rhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("localBindings"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> simpleValueBindingWithPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("rhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("localBindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("localBindings"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> simpleValueBindingWithRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> original, hydra.phantoms.TTerm<hydra.haskell.syntax.RightHandSide> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("pattern"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("localBindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.SimpleValueBinding"), new hydra.core.Name("localBindings"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SpecImport> specImportHiding(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.ImportExportSpec>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.SpecImport"), new hydra.core.Field(new hydra.core.Name("hiding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SpecImport> specImportList(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.ImportExportSpec>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.SpecImport"), new hydra.core.Field(new hydra.core.Name("list"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Statement> statement(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.Statement"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SubspecImportExportSpec> subspecImportExportSpecAll() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.SubspecImportExportSpec"), new hydra.core.Field(new hydra.core.Name("all"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.SubspecImportExportSpec> subspecImportExportSpecList(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Name>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.SubspecImportExportSpec"), new hydra.core.Field(new hydra.core.Name("list"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeApplication(hydra.phantoms.TTerm<hydra.haskell.syntax.ApplicationType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("application"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeCtx(hydra.phantoms.TTerm<hydra.haskell.syntax.ContextType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("ctx"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> typeDeclaration(hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> name, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> typeDeclarationName(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeDeclarationType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> typeDeclarationWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> original, hydra.phantoms.TTerm<hydra.haskell.syntax.DeclarationHead> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> typeDeclarationWithType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeDeclaration> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeDeclaration"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeFunction(hydra.phantoms.TTerm<hydra.haskell.syntax.FunctionType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("function"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeInfix(hydra.phantoms.TTerm<hydra.haskell.syntax.InfixType> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("infix"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeList(hydra.phantoms.TTerm<hydra.haskell.syntax.Type> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("list"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeParens(hydra.phantoms.TTerm<hydra.haskell.syntax.Type> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("parens"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> typeSignature(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> name, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeSignature"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> typeSignatureExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> inner, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), (inner).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> typeSignatureExpressionInner(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), new hydra.core.Name("inner"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeSignatureExpressionType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> typeSignatureExpressionWithInner(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> typeSignatureExpressionWithType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignatureExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression"), new hydra.core.Name("inner"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> typeSignatureName(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignature"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeSignatureType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignature"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> typeSignatureWithName(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeSignature"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignature"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> typeSignatureWithType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypeSignature"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypeSignature"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeTuple(hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Type>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("tuple"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typeVariable(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.Type"), new hydra.core.Field(new hydra.core.Name("variable"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> typedBinding(hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> typeSignature, hydra.phantoms.TTerm<hydra.haskell.syntax.ValueBinding> valueBinding) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypedBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeSignature"), (typeSignature).value),
      new hydra.core.Field(new hydra.core.Name("valueBinding"), (valueBinding).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> typedBindingTypeSignature(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedBinding"), new hydra.core.Name("typeSignature"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ValueBinding> typedBindingValueBinding(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedBinding"), new hydra.core.Name("valueBinding"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> typedBindingWithTypeSignature(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> original, hydra.phantoms.TTerm<hydra.haskell.syntax.TypeSignature> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypedBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeSignature"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("valueBinding"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedBinding"), new hydra.core.Name("valueBinding"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> typedBindingWithValueBinding(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedBinding> original, hydra.phantoms.TTerm<hydra.haskell.syntax.ValueBinding> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypedBinding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("typeSignature"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedBinding"), new hydra.core.Name("typeSignature"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("valueBinding"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> typedPattern(hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> inner, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> type) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypedPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), (inner).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> typedPatternInner(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedPattern"), new hydra.core.Name("inner"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Type> typedPatternType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedPattern"), new hydra.core.Name("type"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> typedPatternWithInner(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Pattern> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypedPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedPattern"), new hydra.core.Name("type"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> typedPatternWithType(hydra.phantoms.TTerm<hydra.haskell.syntax.TypedPattern> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.TypedPattern"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.TypedPattern"), new hydra.core.Name("inner"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> unCaseRhs(hydra.phantoms.TTerm<hydra.haskell.syntax.CaseRhs> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.CaseRhs")), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.Name>> unDeriving(hydra.phantoms.TTerm<hydra.haskell.syntax.Deriving> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.Deriving")), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.LocalBinding>> unLocalBindings(hydra.phantoms.TTerm<hydra.haskell.syntax.LocalBindings> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.LocalBindings")), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unModuleName(hydra.phantoms.TTerm<hydra.haskell.syntax.ModuleName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.ModuleName")), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unNamePart(hydra.phantoms.TTerm<hydra.haskell.syntax.NamePart> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.NamePart")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> unRightHandSide(hydra.phantoms.TTerm<hydra.haskell.syntax.RightHandSide> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.RightHandSide")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> unStatement(hydra.phantoms.TTerm<hydra.haskell.syntax.Statement> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.Statement")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Name> unVariable(hydra.phantoms.TTerm<hydra.haskell.syntax.Variable> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.haskell.syntax.Variable")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> updateRecordExpression(hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> inner, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldUpdate>> fields) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), (inner).value),
      new hydra.core.Field(new hydra.core.Name("fields"), (fields).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldUpdate>> updateRecordExpressionFields(hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), new hydra.core.Name("fields"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> updateRecordExpressionInner(hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), new hydra.core.Name("inner"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> updateRecordExpressionWithFields(hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> original, hydra.phantoms.TTerm<java.util.List<hydra.haskell.syntax.FieldUpdate>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), new hydra.core.Name("inner"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fields"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> updateRecordExpressionWithInner(hydra.phantoms.TTerm<hydra.haskell.syntax.UpdateRecordExpression> original, hydra.phantoms.TTerm<hydra.haskell.syntax.Expression> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("inner"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fields"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.haskell.syntax.UpdateRecordExpression"), new hydra.core.Name("fields"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.ValueBinding> valueBindingSimple(hydra.phantoms.TTerm<hydra.haskell.syntax.SimpleValueBinding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.haskell.syntax.ValueBinding"), new hydra.core.Field(new hydra.core.Name("simple"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.haskell.syntax.Variable> variable(hydra.phantoms.TTerm<hydra.haskell.syntax.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.haskell.syntax.Variable"), (x).value)));
  }
}
