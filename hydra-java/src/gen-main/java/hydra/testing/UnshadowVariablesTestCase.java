// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which renames shadowed variables in a term and compares the result with the expected term
 */
public class UnshadowVariablesTestCase implements Serializable, Comparable<UnshadowVariablesTestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.UnshadowVariablesTestCase");
  
  public static final hydra.core.Name INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term with potentially shadowed variables
   */
  public final hydra.core.Term input;
  
  /**
   * The expected term after unshadowing
   */
  public final hydra.core.Term output;
  
  public UnshadowVariablesTestCase (hydra.core.Term input, hydra.core.Term output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnshadowVariablesTestCase)) {
      return false;
    }
    UnshadowVariablesTestCase o = (UnshadowVariablesTestCase) other;
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
  public int compareTo(UnshadowVariablesTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public UnshadowVariablesTestCase withInput(hydra.core.Term input) {
    return new UnshadowVariablesTestCase(input, output);
  }
  
  public UnshadowVariablesTestCase withOutput(hydra.core.Term output) {
    return new UnshadowVariablesTestCase(input, output);
  }
}
