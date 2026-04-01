// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.testing
 */
public interface Testing {
  static hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> alphaConversionTestCase(hydra.phantoms.TTerm<hydra.core.Term> term, hydra.phantoms.TTerm<hydra.core.Name> oldVariable, hydra.phantoms.TTerm<hydra.core.Name> newVariable, hydra.phantoms.TTerm<hydra.core.Term> result) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), (term).value),
      new hydra.core.Field(new hydra.core.Name("oldVariable"), (oldVariable).value),
      new hydra.core.Field(new hydra.core.Name("newVariable"), (newVariable).value),
      new hydra.core.Field(new hydra.core.Name("result"), (result).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> alphaConversionTestCaseNewVariable(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("newVariable"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> alphaConversionTestCaseOldVariable(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("oldVariable"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> alphaConversionTestCaseResult(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("result"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> alphaConversionTestCaseTerm(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("term"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> alphaConversionTestCaseWithNewVariable(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("oldVariable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("oldVariable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("newVariable"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("result"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("result"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> alphaConversionTestCaseWithOldVariable(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("oldVariable"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("newVariable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("newVariable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("result"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("result"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> alphaConversionTestCaseWithResult(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("oldVariable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("oldVariable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("newVariable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("newVariable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("result"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> alphaConversionTestCaseWithTerm(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("oldVariable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("oldVariable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("newVariable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("newVariable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("result"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.AlphaConversionTestCase"), new hydra.core.Name("result"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> caseConversionTestCase(hydra.phantoms.TTerm<hydra.util.CaseConvention> fromConvention, hydra.phantoms.TTerm<hydra.util.CaseConvention> toConvention, hydra.phantoms.TTerm<String> fromString, hydra.phantoms.TTerm<String> toString) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fromConvention"), (fromConvention).value),
      new hydra.core.Field(new hydra.core.Name("toConvention"), (toConvention).value),
      new hydra.core.Field(new hydra.core.Name("fromString"), (fromString).value),
      new hydra.core.Field(new hydra.core.Name("toString"), (toString).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConversionTestCaseFromConvention(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromConvention"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> caseConversionTestCaseFromString(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromString"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConversionTestCaseToConvention(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toConvention"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> caseConversionTestCaseToString(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toString"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> caseConversionTestCaseWithFromConvention(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> original, hydra.phantoms.TTerm<hydra.util.CaseConvention> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fromConvention"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("toConvention"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toConvention"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fromString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromString"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("toString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toString"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> caseConversionTestCaseWithFromString(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fromConvention"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromConvention"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("toConvention"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toConvention"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fromString"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("toString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toString"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> caseConversionTestCaseWithToConvention(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> original, hydra.phantoms.TTerm<hydra.util.CaseConvention> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fromConvention"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromConvention"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("toConvention"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fromString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromString"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("toString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toString"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> caseConversionTestCaseWithToString(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fromConvention"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromConvention"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("toConvention"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("toConvention"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fromString"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.CaseConversionTestCase"), new hydra.core.Name("fromString"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("toString"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> deannotateTermTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> deannotateTermTestCaseInput(hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> deannotateTermTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> deannotateTermTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> deannotateTermTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> deannotateTypeTestCase(hydra.phantoms.TTerm<hydra.core.Type> input, hydra.phantoms.TTerm<hydra.core.Type> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> deannotateTypeTestCaseInput(hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> deannotateTypeTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> deannotateTypeTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> deannotateTypeTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> delegatedEvaluationTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> delegatedEvaluationTestCaseInput(hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> delegatedEvaluationTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> delegatedEvaluationTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> delegatedEvaluationTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> etaExpansionTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> etaExpansionTestCaseInput(hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> etaExpansionTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> etaExpansionTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> etaExpansionTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EtaExpansionTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EvaluationStyle> evaluationStyleEager() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.EvaluationStyle"), new hydra.core.Field(new hydra.core.Name("eager"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EvaluationStyle> evaluationStyleLazy() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.EvaluationStyle"), new hydra.core.Field(new hydra.core.Name("lazy"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> evaluationTestCase(hydra.phantoms.TTerm<hydra.testing.EvaluationStyle> evaluationStyle, hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("evaluationStyle"), (evaluationStyle).value),
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EvaluationStyle> evaluationTestCaseEvaluationStyle(hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("evaluationStyle"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> evaluationTestCaseInput(hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> evaluationTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> evaluationTestCaseWithEvaluationStyle(hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> original, hydra.phantoms.TTerm<hydra.testing.EvaluationStyle> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("evaluationStyle"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> evaluationTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("evaluationStyle"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("evaluationStyle"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> evaluationTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.EvaluationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("evaluationStyle"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("evaluationStyle"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.EvaluationTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> flattenLetTermsTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> flattenLetTermsTestCaseInput(hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> flattenLetTermsTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> flattenLetTermsTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> flattenLetTermsTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOperation> foldOperationCollectLabels() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.FoldOperation"), new hydra.core.Field(new hydra.core.Name("collectLabels"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOperation> foldOperationCollectListLengths() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.FoldOperation"), new hydra.core.Field(new hydra.core.Name("collectListLengths"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOperation> foldOperationSumInt32Literals() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.FoldOperation"), new hydra.core.Field(new hydra.core.Name("sumInt32Literals"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> foldOverTermTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.coders.TraversalOrder> traversalOrder, hydra.phantoms.TTerm<hydra.testing.FoldOperation> operation, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("traversalOrder"), (traversalOrder).value),
      new hydra.core.Field(new hydra.core.Name("operation"), (operation).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> foldOverTermTestCaseInput(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOperation> foldOverTermTestCaseOperation(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("operation"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> foldOverTermTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.TraversalOrder> foldOverTermTestCaseTraversalOrder(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("traversalOrder"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> foldOverTermTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("traversalOrder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("traversalOrder"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("operation"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> foldOverTermTestCaseWithOperation(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> original, hydra.phantoms.TTerm<hydra.testing.FoldOperation> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("traversalOrder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("traversalOrder"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operation"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> foldOverTermTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("traversalOrder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("traversalOrder"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("operation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("operation"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> foldOverTermTestCaseWithTraversalOrder(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> original, hydra.phantoms.TTerm<hydra.coders.TraversalOrder> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("traversalOrder"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("operation"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("operation"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FoldOverTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> freeVariablesTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.util.PersistentSet<hydra.core.Name>> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> freeVariablesTestCaseInput(hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.PersistentSet<hydra.core.Name>> freeVariablesTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> freeVariablesTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> freeVariablesTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> original, hydra.phantoms.TTerm<hydra.util.PersistentSet<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.FreeVariablesTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> hoistCaseStatementsTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> hoistCaseStatementsTestCaseInput(hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> hoistCaseStatementsTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> hoistCaseStatementsTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> hoistCaseStatementsTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> hoistLetBindingsTestCase(hydra.phantoms.TTerm<hydra.core.Let> input, hydra.phantoms.TTerm<hydra.core.Let> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Let> hoistLetBindingsTestCaseInput(hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Let> hoistLetBindingsTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> hoistLetBindingsTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> original, hydra.phantoms.TTerm<hydra.core.Let> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> hoistLetBindingsTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> original, hydra.phantoms.TTerm<hydra.core.Let> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> hoistPolymorphicLetBindingsTestCase(hydra.phantoms.TTerm<hydra.core.Let> input, hydra.phantoms.TTerm<hydra.core.Let> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Let> hoistPolymorphicLetBindingsTestCaseInput(hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Let> hoistPolymorphicLetBindingsTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> hoistPolymorphicLetBindingsTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> original, hydra.phantoms.TTerm<hydra.core.Let> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> hoistPolymorphicLetBindingsTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> original, hydra.phantoms.TTerm<hydra.core.Let> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPredicate> hoistPredicateApplications() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("applications"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPredicate> hoistPredicateCaseStatements() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("caseStatements"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPredicate> hoistPredicateLists() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("lists"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPredicate> hoistPredicateNothing() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.HoistPredicate"), new hydra.core.Field(new hydra.core.Name("nothing"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> hoistSubtermsTestCase(hydra.phantoms.TTerm<hydra.testing.HoistPredicate> predicate, hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("predicate"), (predicate).value),
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> hoistSubtermsTestCaseInput(hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> hoistSubtermsTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistPredicate> hoistSubtermsTestCasePredicate(hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("predicate"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> hoistSubtermsTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("predicate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("predicate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> hoistSubtermsTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("predicate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("predicate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> hoistSubtermsTestCaseWithPredicate(hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> original, hydra.phantoms.TTerm<hydra.testing.HoistPredicate> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("predicate"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.HoistSubtermsTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.InferenceFailureTestCase> inferenceFailureTestCase(hydra.phantoms.TTerm<hydra.core.Term> input) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.InferenceFailureTestCase"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("input"), (input).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> inferenceFailureTestCaseInput(hydra.phantoms.TTerm<hydra.testing.InferenceFailureTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.InferenceFailureTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.InferenceFailureTestCase> inferenceFailureTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.InferenceFailureTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.InferenceFailureTestCase"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("input"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> inferenceTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.TypeScheme> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.InferenceTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> inferenceTestCaseInput(hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.InferenceTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.TypeScheme> inferenceTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.InferenceTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> inferenceTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.InferenceTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.InferenceTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> inferenceTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> original, hydra.phantoms.TTerm<hydra.core.TypeScheme> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.InferenceTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.InferenceTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> joinTypesTestCase(hydra.phantoms.TTerm<hydra.core.Type> left, hydra.phantoms.TTerm<hydra.core.Type> right, hydra.phantoms.TTerm<hydra.util.Either<java.lang.Void, hydra.util.ConsList<hydra.typing.TypeConstraint>>> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("left"), (left).value),
      new hydra.core.Field(new hydra.core.Name("right"), (right).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Either<java.lang.Void, hydra.util.ConsList<hydra.typing.TypeConstraint>>> joinTypesTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> joinTypesTestCaseLeft(hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("left"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> joinTypesTestCaseRight(hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("right"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> joinTypesTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> original, hydra.phantoms.TTerm<hydra.util.Either<java.lang.Void, hydra.util.ConsList<hydra.typing.TypeConstraint>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("left"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("right"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> joinTypesTestCaseWithLeft(hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("left"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("right"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> joinTypesTestCaseWithRight(hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("left"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JoinTypesTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> jsonDecodeTestCase(hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<hydra.json.model.Value> json, hydra.phantoms.TTerm<hydra.util.Either<String, hydra.core.Term>> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("json"), (json).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Either<String, hydra.core.Term>> jsonDecodeTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.json.model.Value> jsonDecodeTestCaseJson(hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("json"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> jsonDecodeTestCaseType(hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> jsonDecodeTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> original, hydra.phantoms.TTerm<hydra.util.Either<String, hydra.core.Term>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("json"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("json"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> jsonDecodeTestCaseWithJson(hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> original, hydra.phantoms.TTerm<hydra.json.model.Value> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("json"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> jsonDecodeTestCaseWithType(hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("json"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("json"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonDecodeTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> jsonEncodeTestCase(hydra.phantoms.TTerm<hydra.core.Term> term, hydra.phantoms.TTerm<hydra.util.Either<String, hydra.json.model.Value>> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), (term).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Either<String, hydra.json.model.Value>> jsonEncodeTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> jsonEncodeTestCaseTerm(hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), new hydra.core.Name("term"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> jsonEncodeTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> original, hydra.phantoms.TTerm<hydra.util.Either<String, hydra.json.model.Value>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), new hydra.core.Name("term"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> jsonEncodeTestCaseWithTerm(hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonEncodeTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> jsonRoundtripTestCase(hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<hydra.core.Term> term) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("term"), (term).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> jsonRoundtripTestCaseTerm(hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), new hydra.core.Name("term"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> jsonRoundtripTestCaseType(hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> jsonRoundtripTestCaseWithTerm(hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("term"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> jsonRoundtripTestCaseWithType(hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("term"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.JsonRoundtripTestCase"), new hydra.core.Name("term"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> liftLambdaAboveLetTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> liftLambdaAboveLetTestCaseInput(hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> liftLambdaAboveLetTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> liftLambdaAboveLetTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> liftLambdaAboveLetTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> normalizeTypeVariablesTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> normalizeTypeVariablesTestCaseInput(hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> normalizeTypeVariablesTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> normalizeTypeVariablesTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> normalizeTypeVariablesTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.ParserTestCase<A>> parserTestCase(hydra.phantoms.TTerm<String> input, hydra.phantoms.TTerm<hydra.parsing.ParseResult<A>> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ParserTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static <A> hydra.phantoms.TTerm<String> parserTestCaseInput(hydra.phantoms.TTerm<hydra.testing.ParserTestCase<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ParserTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.parsing.ParseResult<A>> parserTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.ParserTestCase<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ParserTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.ParserTestCase<A>> parserTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.ParserTestCase<A>> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ParserTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ParserTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.ParserTestCase<A>> parserTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.ParserTestCase<A>> original, hydra.phantoms.TTerm<hydra.parsing.ParseResult<A>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ParserTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ParserTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> rewriteTermTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.testing.TermRewriter> rewriter, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("rewriter"), (rewriter).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> rewriteTermTestCaseInput(hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> rewriteTermTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TermRewriter> rewriteTermTestCaseRewriter(hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("rewriter"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> rewriteTermTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rewriter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("rewriter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> rewriteTermTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rewriter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("rewriter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> rewriteTermTestCaseWithRewriter(hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> original, hydra.phantoms.TTerm<hydra.testing.TermRewriter> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rewriter"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> rewriteTypeTestCase(hydra.phantoms.TTerm<hydra.core.Type> input, hydra.phantoms.TTerm<hydra.testing.TypeRewriter> rewriter, hydra.phantoms.TTerm<hydra.core.Type> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("rewriter"), (rewriter).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> rewriteTypeTestCaseInput(hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> rewriteTypeTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeRewriter> rewriteTypeTestCaseRewriter(hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("rewriter"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> rewriteTypeTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rewriter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("rewriter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> rewriteTypeTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rewriter"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("rewriter"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> rewriteTypeTestCaseWithRewriter(hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> original, hydra.phantoms.TTerm<hydra.testing.TypeRewriter> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rewriter"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.RewriteTypeTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> serializationTestCase(hydra.phantoms.TTerm<hydra.ast.Expr> input, hydra.phantoms.TTerm<String> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SerializationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> serializationTestCaseInput(hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SerializationTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> serializationTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SerializationTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> serializationTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> original, hydra.phantoms.TTerm<hydra.ast.Expr> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SerializationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SerializationTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> serializationTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SerializationTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SerializationTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> simplifyTermTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> simplifyTermTestCaseInput(hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> simplifyTermTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> simplifyTermTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> simplifyTermTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SimplifyTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> substInTypeTestCase(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> substitution, hydra.phantoms.TTerm<hydra.core.Type> input, hydra.phantoms.TTerm<hydra.core.Type> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("substitution"), (substitution).value),
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> substInTypeTestCaseInput(hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> substInTypeTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> substInTypeTestCaseSubstitution(hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("substitution"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> substInTypeTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("substitution"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("substitution"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> substInTypeTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("substitution"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("substitution"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> substInTypeTestCaseWithSubstitution(hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("substitution"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.SubstInTypeTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.Tag> tag(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.testing.Tag"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TermRewriter> termRewriterReplaceFooWithBar() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TermRewriter"), new hydra.core.Field(new hydra.core.Name("replaceFooWithBar"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TermRewriter> termRewriterReplaceInt32WithInt64() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TermRewriter"), new hydra.core.Field(new hydra.core.Name("replaceInt32WithInt64"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseAlphaConversion(hydra.phantoms.TTerm<hydra.testing.AlphaConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("alphaConversion"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseCaseConversion(hydra.phantoms.TTerm<hydra.testing.CaseConversionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("caseConversion"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseDeannotateTerm(hydra.phantoms.TTerm<hydra.testing.DeannotateTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("deannotateTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseDeannotateType(hydra.phantoms.TTerm<hydra.testing.DeannotateTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("deannotateType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseDelegatedEvaluation(hydra.phantoms.TTerm<hydra.testing.DelegatedEvaluationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("delegatedEvaluation"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseEtaExpansion(hydra.phantoms.TTerm<hydra.testing.EtaExpansionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("etaExpansion"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseEvaluation(hydra.phantoms.TTerm<hydra.testing.EvaluationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("evaluation"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseFlattenLetTerms(hydra.phantoms.TTerm<hydra.testing.FlattenLetTermsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("flattenLetTerms"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseFoldOverTerm(hydra.phantoms.TTerm<hydra.testing.FoldOverTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("foldOverTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseFreeVariables(hydra.phantoms.TTerm<hydra.testing.FreeVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("freeVariables"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseHoistCaseStatements(hydra.phantoms.TTerm<hydra.testing.HoistCaseStatementsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistCaseStatements"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseHoistLetBindings(hydra.phantoms.TTerm<hydra.testing.HoistLetBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistLetBindings"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseHoistPolymorphicLetBindings(hydra.phantoms.TTerm<hydra.testing.HoistPolymorphicLetBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistPolymorphicLetBindings"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseHoistSubterms(hydra.phantoms.TTerm<hydra.testing.HoistSubtermsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("hoistSubterms"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseInference(hydra.phantoms.TTerm<hydra.testing.InferenceTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("inference"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseInferenceFailure(hydra.phantoms.TTerm<hydra.testing.InferenceFailureTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("inferenceFailure"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseJoinTypes(hydra.phantoms.TTerm<hydra.testing.JoinTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("joinTypes"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseJsonDecode(hydra.phantoms.TTerm<hydra.testing.JsonDecodeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonDecode"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseJsonEncode(hydra.phantoms.TTerm<hydra.testing.JsonEncodeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonEncode"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseJsonParser(hydra.phantoms.TTerm<hydra.testing.ParserTestCase<hydra.json.model.Value>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonParser"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseJsonRoundtrip(hydra.phantoms.TTerm<hydra.testing.JsonRoundtripTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonRoundtrip"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseJsonWriter(hydra.phantoms.TTerm<hydra.testing.WriterTestCase<hydra.json.model.Value>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("jsonWriter"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseLiftLambdaAboveLet(hydra.phantoms.TTerm<hydra.testing.LiftLambdaAboveLetTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("liftLambdaAboveLet"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseNormalizeTypeVariables(hydra.phantoms.TTerm<hydra.testing.NormalizeTypeVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("normalizeTypeVariables"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseRewriteTerm(hydra.phantoms.TTerm<hydra.testing.RewriteTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("rewriteTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseRewriteType(hydra.phantoms.TTerm<hydra.testing.RewriteTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("rewriteType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseSerialization(hydra.phantoms.TTerm<hydra.testing.SerializationTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("serialization"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseSimplifyTerm(hydra.phantoms.TTerm<hydra.testing.SimplifyTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("simplifyTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseSubstInType(hydra.phantoms.TTerm<hydra.testing.SubstInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("substInType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseTopologicalSort(hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("topologicalSort"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseTopologicalSortBindings(hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("topologicalSortBindings"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseTopologicalSortSCC(hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("topologicalSortSCC"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseTypeChecking(hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("typeChecking"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseTypeCheckingFailure(hydra.phantoms.TTerm<hydra.testing.TypeCheckingFailureTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("typeCheckingFailure"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseTypeReduction(hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("typeReduction"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseUnifyTypes(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("unifyTypes"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseUniversal(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("universal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseUnshadowVariables(hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("unshadowVariables"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseValidateCoreTerm(hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("validateCoreTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseVariableOccursInType(hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("variableOccursInType"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadata(hydra.phantoms.TTerm<String> name, hydra.phantoms.TTerm<hydra.testing.TestCase> case_, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.Tag>> tags) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("case"), (case_).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value),
      new hydra.core.Field(new hydra.core.Name("tags"), (tags).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseWithMetadataCase(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> testCaseWithMetadataDescription(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testCaseWithMetadataName(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.Tag>> testCaseWithMetadataTags(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithCase(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<hydra.testing.TestCase> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("case"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithDescription(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("case"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithName(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("case"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithTags(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.Tag>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("case"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("tags"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodec(hydra.phantoms.TTerm<hydra.coders.LanguageName> language, hydra.phantoms.TTerm<hydra.module.FileExtension> fileExtension, hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>> encodeTerm, hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>> encodeType, hydra.phantoms.TTerm<java.util.function.Function<String, String>> formatTestName, hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Namespace, String>> formatModuleName, hydra.phantoms.TTerm<String> testCaseTemplate, hydra.phantoms.TTerm<String> testGroupTemplate, hydra.phantoms.TTerm<String> moduleTemplate, hydra.phantoms.TTerm<String> importTemplate, hydra.phantoms.TTerm<java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.ConsList<String>>> findImports) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), (language).value),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), (fileExtension).value),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), (encodeTerm).value),
      new hydra.core.Field(new hydra.core.Name("encodeType"), (encodeType).value),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), (formatTestName).value),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), (formatModuleName).value),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), (testCaseTemplate).value),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), (testGroupTemplate).value),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), (moduleTemplate).value),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), (importTemplate).value),
      new hydra.core.Field(new hydra.core.Name("findImports"), (findImports).value)))));
  }

  static hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>> testCodecEncodeTerm(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>> testCodecEncodeType(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.module.FileExtension> testCodecFileExtension(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.ConsList<String>>> testCodecFindImports(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Namespace, String>> testCodecFormatModuleName(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.function.Function<String, String>> testCodecFormatTestName(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testCodecImportTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageName> testCodecLanguage(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testCodecModuleTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testCodecTestCaseTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testCodecTestGroupTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithEncodeTerm(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithEncodeType(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithFileExtension(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<hydra.module.FileExtension> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithFindImports(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.ConsList<String>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithFormatModuleName(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Namespace, String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithFormatTestName(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<java.util.function.Function<String, String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithImportTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithLanguage(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<hydra.coders.LanguageName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithModuleTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithTestCaseTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testGroupTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCodec> testCodecWithTestGroupTemplate(hydra.phantoms.TTerm<hydra.testing.TestCodec> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCodec"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("language"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("fileExtension"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("fileExtension"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("encodeType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("encodeType"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatTestName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatTestName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("formatModuleName"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("formatModuleName"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testCaseTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("testCaseTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("testGroupTemplate"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("moduleTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("moduleTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("importTemplate"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("importTemplate"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("findImports"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCodec"), new hydra.core.Name("findImports"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> testGenerator(hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.module.Namespaces<A>>>>> namespacesForModule, hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Namespaces<A>, hydra.testing.TestCodec>> createCodec, hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.testing.TestGroup, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.util.Pair<String, String>>>>>> generateTestFile, hydra.phantoms.TTerm<hydra.util.Maybe<java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.module.Module>, hydra.util.Pair<String, String>>>>> aggregatorFile) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGenerator"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespacesForModule"), (namespacesForModule).value),
      new hydra.core.Field(new hydra.core.Name("createCodec"), (createCodec).value),
      new hydra.core.Field(new hydra.core.Name("generateTestFile"), (generateTestFile).value),
      new hydra.core.Field(new hydra.core.Name("aggregatorFile"), (aggregatorFile).value)))));
  }

  static <A> hydra.phantoms.TTerm<hydra.util.Maybe<java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.module.Module>, hydra.util.Pair<String, String>>>>> testGeneratorAggregatorFile(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("aggregatorFile"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Namespaces<A>, hydra.testing.TestCodec>> testGeneratorCreateCodec(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("createCodec"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.testing.TestGroup, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.util.Pair<String, String>>>>>> testGeneratorGenerateTestFile(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("generateTestFile"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.module.Namespaces<A>>>>> testGeneratorNamespacesForModule(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("namespacesForModule"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> testGeneratorWithAggregatorFile(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> original, hydra.phantoms.TTerm<hydra.util.Maybe<java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.module.Module>, hydra.util.Pair<String, String>>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGenerator"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespacesForModule"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("namespacesForModule"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("createCodec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("createCodec"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("generateTestFile"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("generateTestFile"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("aggregatorFile"), (newVal).value)))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> testGeneratorWithCreateCodec(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Namespaces<A>, hydra.testing.TestCodec>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGenerator"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespacesForModule"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("namespacesForModule"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("createCodec"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("generateTestFile"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("generateTestFile"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("aggregatorFile"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("aggregatorFile"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> testGeneratorWithGenerateTestFile(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.testing.TestGroup, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.util.Pair<String, String>>>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGenerator"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespacesForModule"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("namespacesForModule"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("createCodec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("createCodec"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("generateTestFile"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("aggregatorFile"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("aggregatorFile"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> testGeneratorWithNamespacesForModule(hydra.phantoms.TTerm<hydra.testing.TestGenerator<A>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.module.Namespaces<A>>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGenerator"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("namespacesForModule"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("createCodec"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("createCodec"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("generateTestFile"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("generateTestFile"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("aggregatorFile"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGenerator"), new hydra.core.Name("aggregatorFile"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroup(hydra.phantoms.TTerm<String> name, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.TestGroup>> subgroups, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>> cases) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value),
      new hydra.core.Field(new hydra.core.Name("subgroups"), (subgroups).value),
      new hydra.core.Field(new hydra.core.Name("cases"), (cases).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>> testGroupCases(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> testGroupDescription(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testGroupName(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.TestGroup>> testGroupSubgroups(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithCases(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithDescription(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithName(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithSubgroups(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.testing.TestGroup>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> topologicalSortBindingsTestCase(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> bindings, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("bindings"), (bindings).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> topologicalSortBindingsTestCaseBindings(hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), new hydra.core.Name("bindings"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>> topologicalSortBindingsTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> topologicalSortBindingsTestCaseWithBindings(hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("bindings"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> topologicalSortBindingsTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.TopologicalSortBindingsTestCase> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), new hydra.core.Name("bindings"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> topologicalSortSCCTestCase(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>> adjacencyList, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<Integer>>> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), (adjacencyList).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>> topologicalSortSCCTestCaseAdjacencyList(hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), new hydra.core.Name("adjacencyList"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<Integer>>> topologicalSortSCCTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> topologicalSortSCCTestCaseWithAdjacencyList(hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> topologicalSortSCCTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.TopologicalSortSCCTestCase> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.ConsList<Integer>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), new hydra.core.Name("adjacencyList"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> topologicalSortTestCase(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>> adjacencyList, hydra.phantoms.TTerm<hydra.util.Either<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), (adjacencyList).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>> topologicalSortTestCaseAdjacencyList(hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), new hydra.core.Name("adjacencyList"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Either<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>> topologicalSortTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> topologicalSortTestCaseWithAdjacencyList(hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> topologicalSortTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.TopologicalSortTestCase> original, hydra.phantoms.TTerm<hydra.util.Either<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.ConsList<Integer>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("adjacencyList"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TopologicalSortTestCase"), new hydra.core.Name("adjacencyList"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeCheckingFailureTestCase> typeCheckingFailureTestCase(hydra.phantoms.TTerm<hydra.core.Term> input) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("input"), (input).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> typeCheckingFailureTestCaseInput(hydra.phantoms.TTerm<hydra.testing.TypeCheckingFailureTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeCheckingFailureTestCase> typeCheckingFailureTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.TypeCheckingFailureTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("input"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> typeCheckingTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> outputTerm, hydra.phantoms.TTerm<hydra.core.Type> outputType) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("outputTerm"), (outputTerm).value),
      new hydra.core.Field(new hydra.core.Name("outputType"), (outputType).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> typeCheckingTestCaseInput(hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> typeCheckingTestCaseOutputTerm(hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("outputTerm"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeCheckingTestCaseOutputType(hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("outputType"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> typeCheckingTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("outputTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("outputTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("outputType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("outputType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> typeCheckingTestCaseWithOutputTerm(hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("outputTerm"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("outputType"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("outputType"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> typeCheckingTestCaseWithOutputType(hydra.phantoms.TTerm<hydra.testing.TypeCheckingTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("outputTerm"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeCheckingTestCase"), new hydra.core.Name("outputTerm"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("outputType"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> typeReductionTestCase(hydra.phantoms.TTerm<hydra.core.Type> input, hydra.phantoms.TTerm<hydra.core.Type> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeReductionTestCaseInput(hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> typeReductionTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> typeReductionTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> typeReductionTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.TypeReductionTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TypeReductionTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TypeRewriter> typeRewriterReplaceStringWithInt32() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TypeRewriter"), new hydra.core.Field(new hydra.core.Name("replaceStringWithInt32"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<String> unTag(hydra.phantoms.TTerm<hydra.testing.Tag> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.testing.Tag")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> unifyTypesTestCase(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Name>> schemaTypes, hydra.phantoms.TTerm<hydra.core.Type> left, hydra.phantoms.TTerm<hydra.core.Type> right, hydra.phantoms.TTerm<hydra.util.Either<String, hydra.typing.TypeSubst>> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), (schemaTypes).value),
      new hydra.core.Field(new hydra.core.Name("left"), (left).value),
      new hydra.core.Field(new hydra.core.Name("right"), (right).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Either<String, hydra.typing.TypeSubst>> unifyTypesTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unifyTypesTestCaseLeft(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("left"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> unifyTypesTestCaseRight(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("right"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Name>> unifyTypesTestCaseSchemaTypes(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("schemaTypes"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> unifyTypesTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> original, hydra.phantoms.TTerm<hydra.util.Either<String, hydra.typing.TypeSubst>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("left"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("right"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> unifyTypesTestCaseWithLeft(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("left"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("right"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> unifyTypesTestCaseWithRight(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("schemaTypes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("left"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> unifyTypesTestCaseWithSchemaTypes(hydra.phantoms.TTerm<hydra.testing.UnifyTypesTestCase> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.core.Name>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("schemaTypes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("left"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("right"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnifyTypesTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> universalTestCase(hydra.phantoms.TTerm<String> actual, hydra.phantoms.TTerm<String> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UniversalTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("actual"), (actual).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<String> universalTestCaseActual(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("actual"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> universalTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> universalTestCaseWithActual(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UniversalTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("actual"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> universalTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UniversalTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("actual"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("actual"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> unshadowVariablesTestCase(hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.core.Term> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> unshadowVariablesTestCaseInput(hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> unshadowVariablesTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> unshadowVariablesTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> unshadowVariablesTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.UnshadowVariablesTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> validateCoreTermTestCase(hydra.phantoms.TTerm<Boolean> typed, hydra.phantoms.TTerm<hydra.core.Term> input, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.error.core.InvalidTermError>> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typed"), (typed).value),
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Term> validateCoreTermTestCaseInput(hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<hydra.error.core.InvalidTermError>> validateCoreTermTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> validateCoreTermTestCaseTyped(hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("typed"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> validateCoreTermTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> original, hydra.phantoms.TTerm<hydra.core.Term> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typed"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("typed"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> validateCoreTermTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> original, hydra.phantoms.TTerm<hydra.util.Maybe<hydra.error.core.InvalidTermError>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typed"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("typed"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> validateCoreTermTestCaseWithTyped(hydra.phantoms.TTerm<hydra.testing.ValidateCoreTermTestCase> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("typed"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> variableOccursInTypeTestCase(hydra.phantoms.TTerm<hydra.core.Name> variable, hydra.phantoms.TTerm<hydra.core.Type> type, hydra.phantoms.TTerm<Boolean> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variable"), (variable).value),
      new hydra.core.Field(new hydra.core.Name("type"), (type).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<Boolean> variableOccursInTypeTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("expected"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Type> variableOccursInTypeTestCaseType(hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("type"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> variableOccursInTypeTestCaseVariable(hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("variable"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> variableOccursInTypeTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("variable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> variableOccursInTypeTestCaseWithType(hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Type> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variable"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("variable"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("type"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> variableOccursInTypeTestCaseWithVariable(hydra.phantoms.TTerm<hydra.testing.VariableOccursInTypeTestCase> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variable"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("type"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("type"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase"), new hydra.core.Name("expected"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.WriterTestCase<A>> writerTestCase(hydra.phantoms.TTerm<A> input, hydra.phantoms.TTerm<String> output) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.WriterTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (input).value),
      new hydra.core.Field(new hydra.core.Name("output"), (output).value)))));
  }

  static <A> hydra.phantoms.TTerm<A> writerTestCaseInput(hydra.phantoms.TTerm<hydra.testing.WriterTestCase<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.WriterTestCase"), new hydra.core.Name("input"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<String> writerTestCaseOutput(hydra.phantoms.TTerm<hydra.testing.WriterTestCase<A>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.WriterTestCase"), new hydra.core.Name("output"))))), (x).value)));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.WriterTestCase<A>> writerTestCaseWithInput(hydra.phantoms.TTerm<hydra.testing.WriterTestCase<A>> original, hydra.phantoms.TTerm<A> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.WriterTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("output"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.WriterTestCase"), new hydra.core.Name("output"))))), (original).value)))))));
  }

  static <A> hydra.phantoms.TTerm<hydra.testing.WriterTestCase<A>> writerTestCaseWithOutput(hydra.phantoms.TTerm<hydra.testing.WriterTestCase<A>> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.WriterTestCase"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("input"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.testing.WriterTestCase"), new hydra.core.Name("input"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("output"), (newVal).value)))));
  }
}
