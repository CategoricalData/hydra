// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which computes the free variables of a term and compares the result with an expected set of names
 */
public class FreeVariablesTestCase implements Serializable, Comparable<FreeVariablesTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.FreeVariablesTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to analyze
   */
  public final hydra.core.Term input;
  
  /**
   * The expected set of free variable names
   */
  public final java.util.Set<hydra.core.Name> output;
  
  public FreeVariablesTestCase (hydra.core.Term input, java.util.Set<hydra.core.Name> output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FreeVariablesTestCase)) {
      return false;
    }
    FreeVariablesTestCase o = (FreeVariablesTestCase) (other);
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
  public int compareTo(FreeVariablesTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      output.hashCode(),
      other.output.hashCode());
  }
  
  public FreeVariablesTestCase withInput(hydra.core.Term input) {
    return new FreeVariablesTestCase(input, output);
  }
  
  public FreeVariablesTestCase withOutput(java.util.Set<hydra.core.Name> output) {
    return new FreeVariablesTestCase(input, output);
  }
}
