// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which serializes an AST expression to a string and compares it with the expected output
 */
public class SerializationTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SerializationTestCase)) {
      return false;
    }
    SerializationTestCase o = (SerializationTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public SerializationTestCase withInput(hydra.ast.Expr input) {
    java.util.Objects.requireNonNull((input));
    return new SerializationTestCase(input, output);
  }
  
  public SerializationTestCase withOutput(String output) {
    java.util.Objects.requireNonNull((output));
    return new SerializationTestCase(input, output);
  }
}
