// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Shared utility functions for test code generation codecs
 */
public interface Utils {
  static hydra.util.Either<String, hydra.core.Term> inferTerm(hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ic -> hydra.show.Errors.error(((java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.errors.Error_>) (projected -> projected.object)).apply(ic))),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.core.Term>) (x -> (x).term),
      hydra.Inference.inferInGraphContext(
        hydra.Lexical.emptyContext(),
        g,
        term));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.testing.TestCaseWithMetadata> inferTestCase(T0 g, hydra.testing.TestCaseWithMetadata tcm) {
    hydra.util.Maybe<String> desc = (tcm).description;
    String name_ = (tcm).name;
    java.util.List<hydra.testing.Tag> tags_ = (tcm).tags;
    hydra.testing.TestCase tcase = (tcm).case_;
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.testing.TestCase, hydra.testing.TestCaseWithMetadata>) (inferredCase -> new hydra.testing.TestCaseWithMetadata(name_, inferredCase, desc, tags_)),
      hydra.util.Either.<T1, hydra.testing.TestCase>right(tcase));
  }

  static <T0, T1> hydra.util.Either<T1, hydra.testing.TestGroup> inferTestGroupTerms(T0 g, hydra.testing.TestGroup tg) {
    java.util.List<hydra.testing.TestCaseWithMetadata> cases_ = (tg).cases;
    hydra.util.Maybe<String> desc = (tg).description;
    String name_ = (tg).name;
    java.util.List<hydra.testing.TestGroup> subgroups = (tg).subgroups;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<T1, hydra.testing.TestGroup>>) (sg -> hydra.test.Utils.<T0, T1>inferTestGroupTerms(
          g,
          sg)),
        subgroups),
      (java.util.function.Function<java.util.List<hydra.testing.TestGroup>, hydra.util.Either<T1, hydra.testing.TestGroup>>) (inferredSubgroups -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<java.util.List<hydra.testing.TestCaseWithMetadata>, hydra.testing.TestGroup>) (inferredCases -> new hydra.testing.TestGroup(name_, desc, inferredSubgroups, inferredCases)),
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Either<T1, hydra.testing.TestCaseWithMetadata>>) (tc -> hydra.test.Utils.<T0, T1>inferTestCase(
            g,
            tc)),
          cases_))));
  }
}
