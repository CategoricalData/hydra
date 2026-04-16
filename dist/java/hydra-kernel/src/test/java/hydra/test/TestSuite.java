// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Hydra's common test suite, which is designed to run identically in each Hydra implementation; the criterion for a true Hydra implementation is that all test cases pass.
 */
public interface TestSuite {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("common", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.lib.Chars.allTests(),
      hydra.test.lib.Eithers.allTests(),
      hydra.test.lib.Equality.allTests(),
      hydra.test.lib.Lists.allTests(),
      hydra.test.lib.Literals.allTests(),
      hydra.test.lib.Logic.allTests(),
      hydra.test.lib.Maps.allTests(),
      hydra.test.lib.Math_.allTests(),
      hydra.test.lib.Maybes.allTests(),
      hydra.test.lib.Pairs.allTests(),
      hydra.test.lib.Regex.allTests(),
      hydra.test.lib.Sets.allTests(),
      hydra.test.lib.Strings.allTests(),
      hydra.test.Annotations.allTests(),
      hydra.test.checking.All.allTests(),
      hydra.test.Dependencies.allTests(),
      hydra.test.Differentiation.allTests(),
      hydra.test.EtaExpansion.allTests(),
      hydra.test.Formatting.allTests(),
      hydra.test.Generation.allTests(),
      hydra.test.hoisting.All.allTests(),
      hydra.test.inference.All.allTests(),
      hydra.test.json.Roundtrip.allTests(),
      hydra.test.json.Writer.allTests(),
      hydra.test.Reduction.allTests(),
      hydra.test.Rewriting.allTests(),
      hydra.test.Serialization.allTests(),
      hydra.test.Sorting.allTests(),
      hydra.test.Strip.allTests(),
      hydra.test.Substitution.allTests(),
      hydra.test.Unification.allTests(),
      hydra.test.validate.All.allTests(),
      hydra.test.Variables.allTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
