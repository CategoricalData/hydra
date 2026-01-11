// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which hoists subterms into let bindings based on a predicate, and compares the result with the expected term. The predicate decides which subterms at which positions should be extracted into new bindings.
 */
public class HoistSubtermsTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.HoistSubtermsTestCase");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The predicate that determines which subterms to hoist
   */
  public final hydra.testing.HoistPredicate predicate;
  
  /**
   * The input term (must contain a let expression for hoisting to occur)
   */
  public final hydra.core.Term input;
  
  /**
   * The expected output term with hoisted subterms as new bindings
   */
  public final hydra.core.Term output;
  
  public HoistSubtermsTestCase (hydra.testing.HoistPredicate predicate, hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((predicate));
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.predicate = predicate;
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HoistSubtermsTestCase)) {
      return false;
    }
    HoistSubtermsTestCase o = (HoistSubtermsTestCase) (other);
    return predicate.equals(o.predicate) && input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * predicate.hashCode() + 3 * input.hashCode() + 5 * output.hashCode();
  }
  
  public HoistSubtermsTestCase withPredicate(hydra.testing.HoistPredicate predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new HoistSubtermsTestCase(predicate, input, output);
  }
  
  public HoistSubtermsTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new HoistSubtermsTestCase(predicate, input, output);
  }
  
  public HoistSubtermsTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new HoistSubtermsTestCase(predicate, input, output);
  }
}
