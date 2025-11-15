// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs type checking on a given term and compares the result with an expected annotated term and type
 */
public class TypeCheckingTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TypeCheckingTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT_TERM = new hydra.core.Name("outputTerm");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT_TYPE = new hydra.core.Name("outputType");
  
  /**
   * An untyped term on which to perform inference, then type check
   */
  public final hydra.core.Term input;
  
  /**
   * The expected fully annotated System F term after type inference
   */
  public final hydra.core.Term outputTerm;
  
  /**
   * The expected inferred type
   */
  public final hydra.core.Type outputType;
  
  public TypeCheckingTestCase (hydra.core.Term input, hydra.core.Term outputTerm, hydra.core.Type outputType) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((outputTerm));
    java.util.Objects.requireNonNull((outputType));
    this.input = input;
    this.outputTerm = outputTerm;
    this.outputType = outputType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeCheckingTestCase)) {
      return false;
    }
    TypeCheckingTestCase o = (TypeCheckingTestCase) (other);
    return input.equals(o.input) && outputTerm.equals(o.outputTerm) && outputType.equals(o.outputType);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * outputTerm.hashCode() + 5 * outputType.hashCode();
  }
  
  public TypeCheckingTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new TypeCheckingTestCase(input, outputTerm, outputType);
  }
  
  public TypeCheckingTestCase withOutputTerm(hydra.core.Term outputTerm) {
    java.util.Objects.requireNonNull((outputTerm));
    return new TypeCheckingTestCase(input, outputTerm, outputType);
  }
  
  public TypeCheckingTestCase withOutputType(hydra.core.Type outputType) {
    java.util.Objects.requireNonNull((outputType));
    return new TypeCheckingTestCase(input, outputTerm, outputType);
  }
}
