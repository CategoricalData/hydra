// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.util
 */
public interface Util {
  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> adapter(hydra.phantoms.TTerm<Boolean> isLossy, hydra.phantoms.TTerm<T1> source, hydra.phantoms.TTerm<T2> target, hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> coder) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Adapter"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("isLossy"), (isLossy).value),
      new hydra.core.Field(new hydra.core.Name("source"), (source).value),
      new hydra.core.Field(new hydra.core.Name("target"), (target).value),
      new hydra.core.Field(new hydra.core.Name("coder"), (coder).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> adapterCoder(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("coder"))))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<Boolean> adapterIsLossy(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("isLossy"))))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<T1> adapterSource(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("source"))))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<T2> adapterTarget(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("target"))))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> adapterWithCoder(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Adapter"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("isLossy"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("isLossy"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("source"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("target"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("coder"), (newVal).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> adapterWithIsLossy(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Adapter"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("isLossy"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("source"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("target"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("coder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("coder"))))), (original).value)))))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> adapterWithSource(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<T1> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Adapter"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("isLossy"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("isLossy"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("source"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("target"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("coder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("coder"))))), (original).value)))))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> adapterWithTarget(hydra.phantoms.TTerm<hydra.util.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<T2> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Adapter"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("isLossy"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("isLossy"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("source"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("coder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Adapter"), new hydra.core.Name("coder"))))), (original).value)))))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Bicoder<T1, T2, V1, V2>> bicoder(hydra.phantoms.TTerm<java.util.function.Function<T1, hydra.util.Adapter<T1, T2, V1, V2>>> encode, hydra.phantoms.TTerm<java.util.function.Function<T2, hydra.util.Adapter<T2, T1, V2, V1>>> decode) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Bicoder"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), (encode).value),
      new hydra.core.Field(new hydra.core.Name("decode"), (decode).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<java.util.function.Function<T2, hydra.util.Adapter<T2, T1, V2, V1>>> bicoderDecode(hydra.phantoms.TTerm<hydra.util.Bicoder<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Bicoder"), new hydra.core.Name("decode"))))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<java.util.function.Function<T1, hydra.util.Adapter<T1, T2, V1, V2>>> bicoderEncode(hydra.phantoms.TTerm<hydra.util.Bicoder<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Bicoder"), new hydra.core.Name("encode"))))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Bicoder<T1, T2, V1, V2>> bicoderWithDecode(hydra.phantoms.TTerm<hydra.util.Bicoder<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<T2, hydra.util.Adapter<T2, T1, V2, V1>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Bicoder"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Bicoder"), new hydra.core.Name("encode"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("decode"), (newVal).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.util.Bicoder<T1, T2, V1, V2>> bicoderWithEncode(hydra.phantoms.TTerm<hydra.util.Bicoder<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<T1, hydra.util.Adapter<T1, T2, V1, V2>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Bicoder"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Bicoder"), new hydra.core.Name("decode"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionCamel() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("camel"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionLowerSnake() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("lowerSnake"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionPascal() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("pascal"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.CaseConvention> caseConventionUpperSnake() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.CaseConvention"), new hydra.core.Field(new hydra.core.Name("upperSnake"), new hydra.core.Term.Unit()))));
  }

  static <V1, V2> hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> coder(hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V2>>>> encode, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V1>>>> decode) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Coder"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), (encode).value),
      new hydra.core.Field(new hydra.core.Name("decode"), (decode).value)))));
  }

  static <V1, V2> hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V1>>>> coderDecode(hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Coder"), new hydra.core.Name("decode"))))), (x).value)));
  }

  static <V1, V2> hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V2>>>> coderEncode(hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Coder"), new hydra.core.Name("encode"))))), (x).value)));
  }

  static <V1, V2> hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> coderWithDecode(hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V1>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Coder"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Coder"), new hydra.core.Name("encode"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("decode"), (newVal).value)))));
  }

  static <V1, V2> hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> coderWithEncode(hydra.phantoms.TTerm<hydra.util.Coder<V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V2>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.util.Coder"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("encode"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.util.Coder"), new hydra.core.Name("decode"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.util.Comparison> comparisonEqualTo() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("equalTo"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Comparison> comparisonGreaterThan() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("greaterThan"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Comparison> comparisonLessThan() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Comparison"), new hydra.core.Field(new hydra.core.Name("lessThan"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Precision> precisionArbitrary() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Precision"), new hydra.core.Field(new hydra.core.Name("arbitrary"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.util.Precision> precisionBits(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.util.Precision"), new hydra.core.Field(new hydra.core.Name("bits"), (x).value))));
  }
}
