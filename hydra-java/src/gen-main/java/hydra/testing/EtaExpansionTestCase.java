// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs eta expansion (adding missing lambda abstractions) on a given term and compares the result with the expected result
 */
public class EtaExpansionTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.EtaExpansionTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to eta expand
   */
  public final hydra.core.Term input;
  
  /**
   * The expected result
   */
  public final hydra.core.Term output;
  
  public EtaExpansionTestCase (hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EtaExpansionTestCase)) {
      return false;
    }
    EtaExpansionTestCase o = (EtaExpansionTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public EtaExpansionTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new EtaExpansionTestCase(input, output);
  }
  
  public EtaExpansionTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new EtaExpansionTestCase(input, output);
  }
}
