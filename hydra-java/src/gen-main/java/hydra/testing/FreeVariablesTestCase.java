// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which computes the free variables of a term and compares the result with an expected set of names
 */
public class FreeVariablesTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FreeVariablesTestCase)) {
      return false;
    }
    FreeVariablesTestCase o = (FreeVariablesTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public FreeVariablesTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new FreeVariablesTestCase(input, output);
  }
  
  public FreeVariablesTestCase withOutput(java.util.Set<hydra.core.Name> output) {
    java.util.Objects.requireNonNull((output));
    return new FreeVariablesTestCase(input, output);
  }
}
