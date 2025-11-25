// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A simple test case with an input and an expected output
 */
public abstract class TestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TestCase");
  
  public static final hydra.core.Name FIELD_NAME_CASE_CONVERSION = new hydra.core.Name("caseConversion");
  
  public static final hydra.core.Name FIELD_NAME_DELEGATED_EVALUATION = new hydra.core.Name("delegatedEvaluation");
  
  public static final hydra.core.Name FIELD_NAME_ETA_EXPANSION = new hydra.core.Name("etaExpansion");
  
  public static final hydra.core.Name FIELD_NAME_EVALUATION = new hydra.core.Name("evaluation");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE = new hydra.core.Name("inference");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE_FAILURE = new hydra.core.Name("inferenceFailure");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CHECKING = new hydra.core.Name("typeChecking");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CHECKING_FAILURE = new hydra.core.Name("typeCheckingFailure");
  
  private TestCase () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(CaseConversion instance) ;
    
    R visit(DelegatedEvaluation instance) ;
    
    R visit(EtaExpansion instance) ;
    
    R visit(Evaluation instance) ;
    
    R visit(Inference instance) ;
    
    R visit(InferenceFailure instance) ;
    
    R visit(TypeChecking instance) ;
    
    R visit(TypeCheckingFailure instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TestCase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(CaseConversion instance) {
      return otherwise((instance));
    }
    
    default R visit(DelegatedEvaluation instance) {
      return otherwise((instance));
    }
    
    default R visit(EtaExpansion instance) {
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
    
    default R visit(TypeChecking instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeCheckingFailure instance) {
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
   * A delegated evaluation test
   */
  public static final class DelegatedEvaluation extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.DelegatedEvaluationTestCase value;
    
    public DelegatedEvaluation (hydra.testing.DelegatedEvaluationTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DelegatedEvaluation)) {
        return false;
      }
      DelegatedEvaluation o = (DelegatedEvaluation) (other);
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
   * An eta expansion test
   */
  public static final class EtaExpansion extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.EtaExpansionTestCase value;
    
    public EtaExpansion (hydra.testing.EtaExpansionTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EtaExpansion)) {
        return false;
      }
      EtaExpansion o = (EtaExpansion) (other);
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
  
  /**
   * A type checking test
   */
  public static final class TypeChecking extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TypeCheckingTestCase value;
    
    public TypeChecking (hydra.testing.TypeCheckingTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeChecking)) {
        return false;
      }
      TypeChecking o = (TypeChecking) (other);
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
   * A type checking failure test (currently unused)
   */
  public static final class TypeCheckingFailure extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TypeCheckingFailureTestCase value;
    
    public TypeCheckingFailure (hydra.testing.TypeCheckingFailureTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeCheckingFailure)) {
        return false;
      }
      TypeCheckingFailure o = (TypeCheckingFailure) (other);
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
