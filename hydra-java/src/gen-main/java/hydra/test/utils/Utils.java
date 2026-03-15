// Note: this is an automatically generated file. Do not edit.

package hydra.test.utils;

/**
 * Shared utility functions for test code generation codecs
 */
public interface Utils {
  static hydra.util.Either<String, hydra.testing.TestGroup> inferTestGroupTerms(hydra.graph.Graph g, hydra.testing.TestGroup tg) {
    hydra.util.ConsList<hydra.testing.TestCaseWithMetadata> cases_ = (tg).cases;
    hydra.util.Maybe<String> desc = (tg).description;
    String name_ = (tg).name;
    hydra.util.ConsList<hydra.testing.TestGroup> subgroups = (tg).subgroups;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, hydra.testing.TestGroup>>) (sg -> hydra.test.utils.Utils.inferTestGroupTerms(
          g,
          sg)),
        subgroups),
      (java.util.function.Function<hydra.util.ConsList<hydra.testing.TestGroup>, hydra.util.Either<String, hydra.testing.TestGroup>>) (inferredSubgroups -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>, hydra.testing.TestGroup>) (inferredCases -> new hydra.testing.TestGroup(name_, desc, inferredSubgroups, inferredCases)),
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Either<String, hydra.testing.TestCaseWithMetadata>>) (tc -> hydra.test.utils.Utils.inferTestCase(
            g,
            tc)),
          cases_))));
  }
  
  static hydra.util.Either<String, hydra.testing.TestCaseWithMetadata> inferTestCase(hydra.graph.Graph g, hydra.testing.TestCaseWithMetadata tcm) {
    hydra.util.Maybe<String> desc = (tcm).description;
    String name_ = (tcm).name;
    hydra.util.ConsList<hydra.testing.Tag> tags_ = (tcm).tags;
    hydra.testing.TestCase tcase = (tcm).case_;
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.testing.TestCase, hydra.testing.TestCaseWithMetadata>) (inferredCase -> new hydra.testing.TestCaseWithMetadata(name_, inferredCase, desc, tags_)),
      (tcase).accept(new hydra.testing.TestCase.PartialVisitor<>() {
        @Override
        public hydra.util.Either<String, hydra.testing.TestCase> otherwise(hydra.testing.TestCase instance) {
          return hydra.util.Either.<String, hydra.testing.TestCase>right(tcase);
        }
        
        @Override
        public hydra.util.Either<String, hydra.testing.TestCase> visit(hydra.testing.TestCase.DelegatedEvaluation delCase) {
          hydra.core.Term input_ = ((delCase).value).input;
          hydra.core.Term output_ = ((delCase).value).output;
          return hydra.lib.eithers.Bind.apply(
            hydra.test.utils.Utils.inferTerm(
              g,
              input_),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.testing.TestCase>>) (inferredInput -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.testing.TestCase>) (inferredOutput -> new hydra.testing.TestCase.DelegatedEvaluation(new hydra.testing.DelegatedEvaluationTestCase(inferredInput, inferredOutput))),
              hydra.test.utils.Utils.inferTerm(
                g,
                output_))));
        }
      }));
  }
  
  static hydra.util.Either<String, hydra.core.Term> inferTerm(hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.error.Error_>, String>) (ic -> hydra.show.error.Error_.error(((java.util.function.Function<hydra.context.InContext<hydra.error.Error_>, hydra.error.Error_>) (projected -> projected.object)).apply(ic))),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.core.Term>) (x -> (x).term),
      hydra.inference.Inference.inferInGraphContext(
        hydra.lexical.Lexical.emptyContext(),
        g,
        term));
  }
}
