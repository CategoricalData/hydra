// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs type checking on a given term and compares the result with an expected annotated term and type
 */
public class TypeCheckingTestCase implements Serializable, Comparable<TypeCheckingTestCase> {
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
    return java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.outputTerm,
      o.outputTerm) && java.util.Objects.equals(
      this.outputType,
      o.outputType);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(input) + 3 * java.util.Objects.hashCode(outputTerm) + 5 * java.util.Objects.hashCode(outputType);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeCheckingTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (outputTerm)).compareTo(other.outputTerm);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (outputType)).compareTo(other.outputType);
  }
  
  public TypeCheckingTestCase withInput(hydra.core.Term input) {
    return new TypeCheckingTestCase(input, outputTerm, outputType);
  }
  
  public TypeCheckingTestCase withOutputTerm(hydra.core.Term outputTerm) {
    return new TypeCheckingTestCase(input, outputTerm, outputType);
  }
  
  public TypeCheckingTestCase withOutputType(hydra.core.Type outputType) {
    return new TypeCheckingTestCase(input, outputTerm, outputType);
  }
}
