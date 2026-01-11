// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for hoistLetBindings with hoistAll=True, which hoists ALL nested let bindings to the top level of a let term, not just polymorphic ones. This is used for targets like Java that cannot have let expressions in arbitrary positions.
 */
public class HoistLetBindingsTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.HoistLetBindingsTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The input let term
   */
  public final hydra.core.Let input;
  
  /**
   * The expected output let term with all nested bindings hoisted to top
   */
  public final hydra.core.Let output;
  
  public HoistLetBindingsTestCase (hydra.core.Let input, hydra.core.Let output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HoistLetBindingsTestCase)) {
      return false;
    }
    HoistLetBindingsTestCase o = (HoistLetBindingsTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public HoistLetBindingsTestCase withInput(hydra.core.Let input) {
    java.util.Objects.requireNonNull((input));
    return new HoistLetBindingsTestCase(input, output);
  }
  
  public HoistLetBindingsTestCase withOutput(hydra.core.Let output) {
    java.util.Objects.requireNonNull((output));
    return new HoistLetBindingsTestCase(input, output);
  }
}
