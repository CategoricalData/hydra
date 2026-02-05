// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which lifts lambda abstractions above let expressions and compares the result with the expected term
 */
public class LiftLambdaAboveLetTestCase implements Serializable, Comparable<LiftLambdaAboveLetTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to transform
   */
  public final hydra.core.Term input;
  
  /**
   * The expected transformed term
   */
  public final hydra.core.Term output;
  
  public LiftLambdaAboveLetTestCase (hydra.core.Term input, hydra.core.Term output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LiftLambdaAboveLetTestCase)) {
      return false;
    }
    LiftLambdaAboveLetTestCase o = (LiftLambdaAboveLetTestCase) (other);
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
  public int compareTo(LiftLambdaAboveLetTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public LiftLambdaAboveLetTestCase withInput(hydra.core.Term input) {
    return new LiftLambdaAboveLetTestCase(input, output);
  }
  
  public LiftLambdaAboveLetTestCase withOutput(hydra.core.Term output) {
    return new LiftLambdaAboveLetTestCase(input, output);
  }
}
