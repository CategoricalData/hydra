// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which serializes an AST expression to a string and compares it with the expected output
 */
public class SerializationTestCase implements Serializable, Comparable<SerializationTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.SerializationTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The AST expression to serialize
   */
  public final hydra.ast.Expr input;
  
  /**
   * The expected serialized string
   */
  public final String output;
  
  public SerializationTestCase (hydra.ast.Expr input, String output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SerializationTestCase)) {
      return false;
    }
    SerializationTestCase o = (SerializationTestCase) other;
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
  public int compareTo(SerializationTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public SerializationTestCase withInput(hydra.ast.Expr input) {
    return new SerializationTestCase(input, output);
  }
  
  public SerializationTestCase withOutput(String output) {
    return new SerializationTestCase(input, output);
  }
}
