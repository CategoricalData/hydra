// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which hoists subterms into let bindings based on a predicate, and compares the result with the expected term. The predicate decides which subterms at which positions should be extracted into new bindings.
 */
public class HoistSubtermsTestCase implements Serializable, Comparable<HoistSubtermsTestCase> {
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
    return java.util.Objects.equals(
      this.predicate,
      o.predicate) && java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.output,
      o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(predicate) + 3 * java.util.Objects.hashCode(input) + 5 * java.util.Objects.hashCode(output);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(HoistSubtermsTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (predicate)).compareTo(other.predicate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public HoistSubtermsTestCase withPredicate(hydra.testing.HoistPredicate predicate) {
    return new HoistSubtermsTestCase(predicate, input, output);
  }
  
  public HoistSubtermsTestCase withInput(hydra.core.Term input) {
    return new HoistSubtermsTestCase(predicate, input, output);
  }
  
  public HoistSubtermsTestCase withOutput(hydra.core.Term output) {
    return new HoistSubtermsTestCase(predicate, input, output);
  }
}
