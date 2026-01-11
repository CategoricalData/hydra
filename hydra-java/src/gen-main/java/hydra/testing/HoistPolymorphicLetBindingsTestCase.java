// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for the hoistPolymorphicLetBindings function, which hoists polymorphic let bindings to the top level of a let term. This is used for targets like Java which don't support polymorphic lambdas.
 */
public class HoistPolymorphicLetBindingsTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The input let term
   */
  public final hydra.core.Let input;
  
  /**
   * The expected output let term with polymorphic bindings hoisted to top
   */
  public final hydra.core.Let output;
  
  public HoistPolymorphicLetBindingsTestCase (hydra.core.Let input, hydra.core.Let output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HoistPolymorphicLetBindingsTestCase)) {
      return false;
    }
    HoistPolymorphicLetBindingsTestCase o = (HoistPolymorphicLetBindingsTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public HoistPolymorphicLetBindingsTestCase withInput(hydra.core.Let input) {
    java.util.Objects.requireNonNull((input));
    return new HoistPolymorphicLetBindingsTestCase(input, output);
  }
  
  public HoistPolymorphicLetBindingsTestCase withOutput(hydra.core.Let output) {
    java.util.Objects.requireNonNull((output));
    return new HoistPolymorphicLetBindingsTestCase(input, output);
  }
}
