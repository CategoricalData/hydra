// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A simple test case with an input and an expected output
 */
public abstract class TestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TestCase");
  
  public static final hydra.core.Name FIELD_NAME_CASE_CONVERSION = new hydra.core.Name("caseConversion");
  
  public static final hydra.core.Name FIELD_NAME_EVALUATION = new hydra.core.Name("evaluation");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE = new hydra.core.Name("inference");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE_FAILURE = new hydra.core.Name("inferenceFailure");
  
  private TestCase () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(CaseConversion instance) ;
    
    R visit(Evaluation instance) ;
    
    R visit(Inference instance) ;
    
    R visit(InferenceFailure instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TestCase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(CaseConversion instance) {
      return otherwise((instance));
    }
    
    default R visit(Evaluation instance) {
      return otherwise((instance));
    }
    
    default R visit(Inference instance) {
      return otherwise((instance));
    }
    
    default R visit(InferenceFailure instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A case conversion test
   */
  public static final class CaseConversion extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.CaseConversionTestCase value;
    
    public CaseConversion (hydra.testing.CaseConversionTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseConversion)) {
        return false;
      }
      CaseConversion o = (CaseConversion) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A term evaluation test
   */
  public static final class Evaluation extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.EvaluationTestCase value;
    
    public Evaluation (hydra.testing.EvaluationTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Evaluation)) {
        return false;
      }
      Evaluation o = (Evaluation) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type inference test
   */
  public static final class Inference extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.InferenceTestCase value;
    
    public Inference (hydra.testing.InferenceTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inference)) {
        return false;
      }
      Inference o = (Inference) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type inference failure test
   */
  public static final class InferenceFailure extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.InferenceFailureTestCase value;
    
    public InferenceFailure (hydra.testing.InferenceFailureTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InferenceFailure)) {
        return false;
      }
      InferenceFailure o = (InferenceFailure) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
