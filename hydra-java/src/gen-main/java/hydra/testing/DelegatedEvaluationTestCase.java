// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * DEPRECATED: Delegated evaluation test case (to be removed)
 */
public class DelegatedEvaluationTestCase implements Serializable, Comparable<DelegatedEvaluationTestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase");

  public static final hydra.core.Name INPUT = new hydra.core.Name("input");

  public static final hydra.core.Name OUTPUT = new hydra.core.Name("output");

  public final hydra.core.Term input;

  public final hydra.core.Term output;

  public DelegatedEvaluationTestCase (hydra.core.Term input, hydra.core.Term output) {
    this.input = input;
    this.output = output;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DelegatedEvaluationTestCase)) {
      return false;
    }
    DelegatedEvaluationTestCase o = (DelegatedEvaluationTestCase) other;
    return java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.output,
      o.output);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(input) + 3 * java.util.Objects.hashCode(output);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DelegatedEvaluationTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }

  public DelegatedEvaluationTestCase withInput(hydra.core.Term input) {
    return new DelegatedEvaluationTestCase(input, output);
  }

  public DelegatedEvaluationTestCase withOutput(hydra.core.Term output) {
    return new DelegatedEvaluationTestCase(input, output);
  }
}
