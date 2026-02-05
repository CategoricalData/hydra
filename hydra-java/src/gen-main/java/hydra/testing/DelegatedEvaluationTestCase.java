// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case in which we delegate evaluation of an input term and an expected output term to a target programming language like Haskell, Java, or Python, checking whether the term evaluates as expected when translated into that language
 */
public class DelegatedEvaluationTestCase implements Serializable, Comparable<DelegatedEvaluationTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The first of two terms which should evaluate to the same expression
   */
  public final hydra.core.Term input;
  
  /**
   * The second of two terms which should evaluate to the same expression
   */
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
    DelegatedEvaluationTestCase o = (DelegatedEvaluationTestCase) (other);
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
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public DelegatedEvaluationTestCase withInput(hydra.core.Term input) {
    return new DelegatedEvaluationTestCase(input, output);
  }
  
  public DelegatedEvaluationTestCase withOutput(hydra.core.Term output) {
    return new DelegatedEvaluationTestCase(input, output);
  }
}
