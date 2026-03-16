// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which computes the free variables of a term and compares the result with an expected set of names
 */
public class FreeVariablesTestCase implements Serializable, Comparable<FreeVariablesTestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.FreeVariablesTestCase");
  
  public static final hydra.core.Name INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to analyze
   */
  public final hydra.core.Term input;
  
  /**
   * The expected set of free variable names
   */
  public final hydra.util.PersistentSet<hydra.core.Name> output;
  
  public FreeVariablesTestCase (hydra.core.Term input, hydra.util.PersistentSet<hydra.core.Name> output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FreeVariablesTestCase)) {
      return false;
    }
    FreeVariablesTestCase o = (FreeVariablesTestCase) other;
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
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public FreeVariablesTestCase withInput(hydra.core.Term input) {
    return new FreeVariablesTestCase(input, output);
  }
  
  public FreeVariablesTestCase withOutput(hydra.util.PersistentSet<hydra.core.Name> output) {
    return new FreeVariablesTestCase(input, output);
  }
}
