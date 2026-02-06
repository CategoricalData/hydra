// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which applies a type substitution to a type and compares the result. The substitution is provided as a list of (variable name, replacement type) pairs.
 */
public class SubstInTypeTestCase implements Serializable, Comparable<SubstInTypeTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.SubstInTypeTestCase");
  
  public static final hydra.core.Name FIELD_NAME_SUBSTITUTION = new hydra.core.Name("substitution");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The type substitution as a list of (name, type) pairs
   */
  public final java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>> substitution;
  
  /**
   * The type to substitute into
   */
  public final hydra.core.Type input;
  
  /**
   * The expected result type
   */
  public final hydra.core.Type output;
  
  public SubstInTypeTestCase (java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>> substitution, hydra.core.Type input, hydra.core.Type output) {
    this.substitution = substitution;
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubstInTypeTestCase)) {
      return false;
    }
    SubstInTypeTestCase o = (SubstInTypeTestCase) (other);
    return java.util.Objects.equals(
      this.substitution,
      o.substitution) && java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.output,
      o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(substitution) + 3 * java.util.Objects.hashCode(input) + 5 * java.util.Objects.hashCode(output);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SubstInTypeTestCase other) {
    int cmp = 0;
    cmp = Integer.compare(
      substitution.hashCode(),
      other.substitution.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public SubstInTypeTestCase withSubstitution(java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>> substitution) {
    return new SubstInTypeTestCase(substitution, input, output);
  }
  
  public SubstInTypeTestCase withInput(hydra.core.Type input) {
    return new SubstInTypeTestCase(substitution, input, output);
  }
  
  public SubstInTypeTestCase withOutput(hydra.core.Type output) {
    return new SubstInTypeTestCase(substitution, input, output);
  }
}
