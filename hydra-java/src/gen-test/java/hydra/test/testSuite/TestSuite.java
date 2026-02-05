// Note: this is an automatically generated file. Do not edit.

package hydra.test.testSuite;

/**
 * Hydra's common test suite, which is designed to run identically in each Hydra implementation; the criterion for a true Hydra implementation is that all test cases pass.
 */
public interface TestSuite {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("common", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(
      hydra.test.lib.chars.Chars.allTests(),
      hydra.test.lib.eithers.Eithers.allTests(),
      hydra.test.lib.equality.Equality.allTests(),
      hydra.test.lib.flows.Flows.allTests(),
      hydra.test.lib.lists.Lists.allTests(),
      hydra.test.lib.literals.Literals.allTests(),
      hydra.test.lib.logic.Logic.allTests(),
      hydra.test.lib.maps.Maps.allTests(),
      hydra.test.lib.math.Math_.allTests(),
      hydra.test.lib.maybes.Maybes.allTests(),
      hydra.test.lib.pairs.Pairs.allTests(),
      hydra.test.lib.sets.Sets.allTests(),
      hydra.test.lib.strings.Strings.allTests(),
      hydra.test.annotations.Annotations.allTests(),
      hydra.test.checking.all.All.allTests(),
      hydra.test.etaExpansion.EtaExpansion.allTests(),
      hydra.test.formatting.Formatting.allTests(),
      hydra.test.hoisting.Hoisting.allTests(),
      hydra.test.inference.all.All.allTests(),
      hydra.test.json.coder.Coder.allTests(),
      hydra.test.json.parser.Parser.allTests(),
      hydra.test.json.roundtrip.Roundtrip.allTests(),
      hydra.test.json.writer.Writer.allTests(),
      hydra.test.monads.Monads.allTests(),
      hydra.test.reduction.Reduction.allTests(),
      hydra.test.rewriting.Rewriting.allTests(),
      hydra.test.serialization.Serialization.allTests(),
      hydra.test.sorting.Sorting.allTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}
