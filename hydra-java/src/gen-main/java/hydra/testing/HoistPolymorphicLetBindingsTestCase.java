// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for the hoistPolymorphicLetBindings function, which hoists polymorphic let bindings to the top level of a let term. This is used for targets like Java which don't support polymorphic lambdas.
 */
public class HoistPolymorphicLetBindingsTestCase implements Serializable, Comparable<HoistPolymorphicLetBindingsTestCase> {
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
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HoistPolymorphicLetBindingsTestCase)) {
      return false;
    }
    HoistPolymorphicLetBindingsTestCase o = (HoistPolymorphicLetBindingsTestCase) other;
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
  public int compareTo(HoistPolymorphicLetBindingsTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public HoistPolymorphicLetBindingsTestCase withInput(hydra.core.Let input) {
    return new HoistPolymorphicLetBindingsTestCase(input, output);
  }
  
  public HoistPolymorphicLetBindingsTestCase withOutput(hydra.core.Let output) {
    return new HoistPolymorphicLetBindingsTestCase(input, output);
  }
}
