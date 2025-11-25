// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case in which we delegate evaluation of an input term and an expected output term to a target programming language like Haskell, Java, or Python, checking whether the term evaluates as expected when translated into that language
 */
public class DelegatedEvaluationTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DelegatedEvaluationTestCase)) {
      return false;
    }
    DelegatedEvaluationTestCase o = (DelegatedEvaluationTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public DelegatedEvaluationTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new DelegatedEvaluationTestCase(input, output);
  }
  
  public DelegatedEvaluationTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new DelegatedEvaluationTestCase(input, output);
  }
}
