// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.coders
 */
public interface Coders {
  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> adapter(hydra.phantoms.TTerm<Boolean> isLossy, hydra.phantoms.TTerm<T1> source, hydra.phantoms.TTerm<T2> target, hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> coder) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Adapter"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("isLossy"), (isLossy).value),
      new hydra.core.Field(new hydra.core.Name("source"), (source).value),
      new hydra.core.Field(new hydra.core.Name("target"), (target).value),
      new hydra.core.Field(new hydra.core.Name("coder"), (coder).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> adapterCoder(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("coder"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.AdapterContext> adapterContext(hydra.phantoms.TTerm<hydra.graph.Graph> graph, hydra.phantoms.TTerm<hydra.coders.Language> language, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> adapters) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.AdapterContext"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("graph"), (graph).value),
      new hydra.core.Field(new hydra.core.Name("language"), (language).value),
      new hydra.core.Field(new hydra.core.Name("adapters"), (adapters).value)))));
  }

  static hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> adapterContextAdapters(hydra.phantoms.TTerm<hydra.coders.AdapterContext> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("adapters"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.graph.Graph> adapterContextGraph(hydra.phantoms.TTerm<hydra.coders.AdapterContext> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("graph"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.Language> adapterContextLanguage(hydra.phantoms.TTerm<hydra.coders.AdapterContext> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("language"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.AdapterContext> adapterContextWithAdapters(hydra.phantoms.TTerm<hydra.coders.AdapterContext> original, hydra.phantoms.TTerm<java.util.Map<hydra.core.Name, hydra.coders.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.AdapterContext"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("graph"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("graph"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("language"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("adapters"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.coders.AdapterContext> adapterContextWithGraph(hydra.phantoms.TTerm<hydra.coders.AdapterContext> original, hydra.phantoms.TTerm<hydra.graph.Graph> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.AdapterContext"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("graph"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("language"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("language"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("adapters"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("adapters"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.AdapterContext> adapterContextWithLanguage(hydra.phantoms.TTerm<hydra.coders.AdapterContext> original, hydra.phantoms.TTerm<hydra.coders.Language> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.AdapterContext"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("graph"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("graph"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("language"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("adapters"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.AdapterContext"), new hydra.core.Name("adapters"))), (original).value)))))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<Boolean> adapterIsLossy(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("isLossy"))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<T1> adapterSource(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("source"))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<T2> adapterTarget(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("target"))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> adapterWithCoder(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Adapter"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("isLossy"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("isLossy"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("source"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("target"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("coder"), (newVal).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> adapterWithIsLossy(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Adapter"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("isLossy"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("source"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("target"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("coder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("coder"))), (original).value)))))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> adapterWithSource(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<T1> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Adapter"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("isLossy"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("isLossy"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("source"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("target"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("coder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("coder"))), (original).value)))))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> adapterWithTarget(hydra.phantoms.TTerm<hydra.coders.Adapter<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<T2> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Adapter"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("isLossy"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("isLossy"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("source"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("coder"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Adapter"), new hydra.core.Name("coder"))), (original).value)))))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Bicoder<T1, T2, V1, V2>> bicoder(hydra.phantoms.TTerm<java.util.function.Function<T1, hydra.coders.Adapter<T1, T2, V1, V2>>> encode, hydra.phantoms.TTerm<java.util.function.Function<T2, hydra.coders.Adapter<T2, T1, V2, V1>>> decode) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Bicoder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("encode"), (encode).value),
      new hydra.core.Field(new hydra.core.Name("decode"), (decode).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<java.util.function.Function<T2, hydra.coders.Adapter<T2, T1, V2, V1>>> bicoderDecode(hydra.phantoms.TTerm<hydra.coders.Bicoder<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Bicoder"), new hydra.core.Name("decode"))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<java.util.function.Function<T1, hydra.coders.Adapter<T1, T2, V1, V2>>> bicoderEncode(hydra.phantoms.TTerm<hydra.coders.Bicoder<T1, T2, V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Bicoder"), new hydra.core.Name("encode"))), (x).value)));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Bicoder<T1, T2, V1, V2>> bicoderWithDecode(hydra.phantoms.TTerm<hydra.coders.Bicoder<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<T2, hydra.coders.Adapter<T2, T1, V2, V1>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Bicoder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Bicoder"), new hydra.core.Name("encode"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("decode"), (newVal).value)))));
  }

  static <T1, T2, V1, V2> hydra.phantoms.TTerm<hydra.coders.Bicoder<T1, T2, V1, V2>> bicoderWithEncode(hydra.phantoms.TTerm<hydra.coders.Bicoder<T1, T2, V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<T1, hydra.coders.Adapter<T1, T2, V1, V2>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Bicoder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("encode"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Bicoder"), new hydra.core.Name("decode"))), (original).value)))))));
  }

  static <V1, V2> hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> coder(hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.errors.Error_, V2>>>> encode, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.errors.Error_, V1>>>> decode) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Coder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("encode"), (encode).value),
      new hydra.core.Field(new hydra.core.Name("decode"), (decode).value)))));
  }

  static <V1, V2> hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.errors.Error_, V1>>>> coderDecode(hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Coder"), new hydra.core.Name("decode"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.CoderDirection> coderDirectionDecode() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.coders.CoderDirection"), new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.coders.CoderDirection> coderDirectionEncode() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.coders.CoderDirection"), new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Unit()))));
  }

  static <V1, V2> hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.errors.Error_, V2>>>> coderEncode(hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Coder"), new hydra.core.Name("encode"))), (x).value)));
  }

  static <V1, V2> hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> coderWithDecode(hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V2, hydra.util.Either<hydra.errors.Error_, V1>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Coder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Coder"), new hydra.core.Name("encode"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("decode"), (newVal).value)))));
  }

  static <V1, V2> hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> coderWithEncode(hydra.phantoms.TTerm<hydra.coders.Coder<V1, V2>> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.context.Context, java.util.function.Function<V1, hydra.util.Either<hydra.errors.Error_, V2>>>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Coder"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("encode"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Coder"), new hydra.core.Name("decode"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.Language> language(hydra.phantoms.TTerm<hydra.coders.LanguageName> name, hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> constraints) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Language"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("constraints"), (constraints).value)))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraints(hydra.phantoms.TTerm<hydra.coders.Language> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Language"), new hydra.core.Name("constraints"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.variants.EliminationVariant>> languageConstraintsEliminationVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.core.FloatType>> languageConstraintsFloatTypes(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.variants.FunctionVariant>> languageConstraintsFunctionVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.core.IntegerType>> languageConstraintsIntegerTypes(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.variants.LiteralVariant>> languageConstraintsLiteralVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.variants.TermVariant>> languageConstraintsTermVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.Set<hydra.variants.TypeVariant>> languageConstraintsTypeVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Type, Boolean>> languageConstraintsTypes(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithEliminationVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.Set<hydra.variants.EliminationVariant>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithFloatTypes(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.Set<hydra.core.FloatType>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithFunctionVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.Set<hydra.variants.FunctionVariant>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithIntegerTypes(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.Set<hydra.core.IntegerType>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("termVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithLiteralVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.Set<hydra.variants.LiteralVariant>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithTermVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.Set<hydra.variants.TermVariant>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termVariants"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithTypeVariants(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.Set<hydra.variants.TypeVariant>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("types"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraintsWithTypes(hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> original, hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Type, Boolean>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("eliminationVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("literalVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("floatTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("functionVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("integerTypes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("termVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("termVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.LanguageConstraints"), new hydra.core.Name("typeVariants"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("types"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> languageConstraints_(hydra.phantoms.TTerm<java.util.Set<hydra.variants.EliminationVariant>> eliminationVariants, hydra.phantoms.TTerm<java.util.Set<hydra.variants.LiteralVariant>> literalVariants, hydra.phantoms.TTerm<java.util.Set<hydra.core.FloatType>> floatTypes, hydra.phantoms.TTerm<java.util.Set<hydra.variants.FunctionVariant>> functionVariants, hydra.phantoms.TTerm<java.util.Set<hydra.core.IntegerType>> integerTypes, hydra.phantoms.TTerm<java.util.Set<hydra.variants.TermVariant>> termVariants, hydra.phantoms.TTerm<java.util.Set<hydra.variants.TypeVariant>> typeVariants, hydra.phantoms.TTerm<java.util.function.Function<hydra.core.Type, Boolean>> types) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.LanguageConstraints"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("eliminationVariants"), (eliminationVariants).value),
      new hydra.core.Field(new hydra.core.Name("literalVariants"), (literalVariants).value),
      new hydra.core.Field(new hydra.core.Name("floatTypes"), (floatTypes).value),
      new hydra.core.Field(new hydra.core.Name("functionVariants"), (functionVariants).value),
      new hydra.core.Field(new hydra.core.Name("integerTypes"), (integerTypes).value),
      new hydra.core.Field(new hydra.core.Name("termVariants"), (termVariants).value),
      new hydra.core.Field(new hydra.core.Name("typeVariants"), (typeVariants).value),
      new hydra.core.Field(new hydra.core.Name("types"), (types).value)))));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageName> languageName(hydra.phantoms.TTerm<hydra.coders.Language> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Language"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.LanguageName> languageName_(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.coders.LanguageName"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.coders.Language> languageWithConstraints(hydra.phantoms.TTerm<hydra.coders.Language> original, hydra.phantoms.TTerm<hydra.coders.LanguageConstraints> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Language"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Language"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("constraints"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.coders.Language> languageWithName(hydra.phantoms.TTerm<hydra.coders.Language> original, hydra.phantoms.TTerm<hydra.coders.LanguageName> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.coders.Language"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("constraints"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.coders.Language"), new hydra.core.Name("constraints"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.coders.TraversalOrder> traversalOrderPost() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.coders.TraversalOrder"), new hydra.core.Field(new hydra.core.Name("post"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.coders.TraversalOrder> traversalOrderPre() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.coders.TraversalOrder"), new hydra.core.Field(new hydra.core.Name("pre"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<String> unLanguageName(hydra.phantoms.TTerm<hydra.coders.LanguageName> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.coders.LanguageName")), (x).value)));
  }
}
