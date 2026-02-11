// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs eta expansion (adding missing lambda abstractions) on a given term and compares the result with the expected result
 */
public class EtaExpansionTestCase implements Serializable, Comparable<EtaExpansionTestCase> {
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
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EtaExpansionTestCase)) {
      return false;
    }
    EtaExpansionTestCase o = (EtaExpansionTestCase) other;
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
  public int compareTo(EtaExpansionTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public EtaExpansionTestCase withInput(hydra.core.Term input) {
    return new EtaExpansionTestCase(input, output);
  }
  
  public EtaExpansionTestCase withOutput(hydra.core.Term output) {
    return new EtaExpansionTestCase(input, output);
  }
}
