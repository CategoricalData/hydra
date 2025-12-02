// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which lifts lambda abstractions above let expressions and compares the result with the expected term
 */
public class LiftLambdaAboveLetTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LiftLambdaAboveLetTestCase)) {
      return false;
    }
    LiftLambdaAboveLetTestCase o = (LiftLambdaAboveLetTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public LiftLambdaAboveLetTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new LiftLambdaAboveLetTestCase(input, output);
  }
  
  public LiftLambdaAboveLetTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new LiftLambdaAboveLetTestCase(input, output);
  }
}
