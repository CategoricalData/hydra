// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.util
 */
public interface Util {
  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionCamel() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("camel"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionLowerSnake() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("lowerSnake"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionPascal() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("pascal"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionUpperSnake() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("upperSnake"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Comparison> comparisonEqualTo() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Comparison> comparisonGreaterThan() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Comparison> comparisonLessThan() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Precision> precisionArbitrary() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.Precision"), new hydra.core.Field(new hydra.core.Name("arbitrary"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Precision> precisionBits(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.util.Precision"), new hydra.core.Field(new hydra.core.Name("bits"), (x).value))));
  }
}
