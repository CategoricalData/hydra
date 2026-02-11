// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A simple test case with an input and an expected output
 */
public abstract class TestCase implements Serializable, Comparable<TestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TestCase");
  
  public static final hydra.core.Name FIELD_NAME_ALPHA_CONVERSION = new hydra.core.Name("alphaConversion");
  
  public static final hydra.core.Name FIELD_NAME_CASE_CONVERSION = new hydra.core.Name("caseConversion");
  
  public static final hydra.core.Name FIELD_NAME_DEANNOTATE_TERM = new hydra.core.Name("deannotateTerm");
  
  public static final hydra.core.Name FIELD_NAME_DEANNOTATE_TYPE = new hydra.core.Name("deannotateType");
  
  public static final hydra.core.Name FIELD_NAME_DELEGATED_EVALUATION = new hydra.core.Name("delegatedEvaluation");
  
  public static final hydra.core.Name FIELD_NAME_ETA_EXPANSION = new hydra.core.Name("etaExpansion");
  
  public static final hydra.core.Name FIELD_NAME_FLATTEN_LET_TERMS = new hydra.core.Name("flattenLetTerms");
  
  public static final hydra.core.Name FIELD_NAME_FREE_VARIABLES = new hydra.core.Name("freeVariables");
  
  public static final hydra.core.Name FIELD_NAME_EVALUATION = new hydra.core.Name("evaluation");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE = new hydra.core.Name("inference");
  
  public static final hydra.core.Name FIELD_NAME_INFERENCE_FAILURE = new hydra.core.Name("inferenceFailure");
  
  public static final hydra.core.Name FIELD_NAME_JSON_CODER = new hydra.core.Name("jsonCoder");
  
  public static final hydra.core.Name FIELD_NAME_JSON_DECODE = new hydra.core.Name("jsonDecode");
  
  public static final hydra.core.Name FIELD_NAME_JSON_ENCODE = new hydra.core.Name("jsonEncode");
  
  public static final hydra.core.Name FIELD_NAME_JSON_PARSER = new hydra.core.Name("jsonParser");
  
  public static final hydra.core.Name FIELD_NAME_JSON_ROUNDTRIP = new hydra.core.Name("jsonRoundtrip");
  
  public static final hydra.core.Name FIELD_NAME_JSON_WRITER = new hydra.core.Name("jsonWriter");
  
  public static final hydra.core.Name FIELD_NAME_LIFT_LAMBDA_ABOVE_LET = new hydra.core.Name("liftLambdaAboveLet");
  
  public static final hydra.core.Name FIELD_NAME_SERIALIZATION = new hydra.core.Name("serialization");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLIFY_TERM = new hydra.core.Name("simplifyTerm");
  
  public static final hydra.core.Name FIELD_NAME_TOPOLOGICAL_SORT = new hydra.core.Name("topologicalSort");
  
  public static final hydra.core.Name FIELD_NAME_TOPOLOGICAL_SORT_BINDINGS = new hydra.core.Name("topologicalSortBindings");
  
  public static final hydra.core.Name FIELD_NAME_TOPOLOGICAL_SORT_S_C_C = new hydra.core.Name("topologicalSortSCC");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CHECKING = new hydra.core.Name("typeChecking");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CHECKING_FAILURE = new hydra.core.Name("typeCheckingFailure");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_REDUCTION = new hydra.core.Name("typeReduction");
  
  public static final hydra.core.Name FIELD_NAME_NORMALIZE_TYPE_VARIABLES = new hydra.core.Name("normalizeTypeVariables");
  
  public static final hydra.core.Name FIELD_NAME_FOLD_OVER_TERM = new hydra.core.Name("foldOverTerm");
  
  public static final hydra.core.Name FIELD_NAME_REWRITE_TERM = new hydra.core.Name("rewriteTerm");
  
  public static final hydra.core.Name FIELD_NAME_REWRITE_TYPE = new hydra.core.Name("rewriteType");
  
  public static final hydra.core.Name FIELD_NAME_HOIST_SUBTERMS = new hydra.core.Name("hoistSubterms");
  
  public static final hydra.core.Name FIELD_NAME_HOIST_CASE_STATEMENTS = new hydra.core.Name("hoistCaseStatements");
  
  public static final hydra.core.Name FIELD_NAME_HOIST_LET_BINDINGS = new hydra.core.Name("hoistLetBindings");
  
  public static final hydra.core.Name FIELD_NAME_HOIST_POLYMORPHIC_LET_BINDINGS = new hydra.core.Name("hoistPolymorphicLetBindings");
  
  public static final hydra.core.Name FIELD_NAME_SUBST_IN_TYPE = new hydra.core.Name("substInType");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE_OCCURS_IN_TYPE = new hydra.core.Name("variableOccursInType");
  
  public static final hydra.core.Name FIELD_NAME_UNIFY_TYPES = new hydra.core.Name("unifyTypes");
  
  public static final hydra.core.Name FIELD_NAME_JOIN_TYPES = new hydra.core.Name("joinTypes");
  
  private TestCase () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AlphaConversion instance) ;
    
    R visit(CaseConversion instance) ;
    
    R visit(DeannotateTerm instance) ;
    
    R visit(DeannotateType instance) ;
    
    R visit(DelegatedEvaluation instance) ;
    
    R visit(EtaExpansion instance) ;
    
    R visit(FlattenLetTerms instance) ;
    
    R visit(FreeVariables instance) ;
    
    R visit(Evaluation instance) ;
    
    R visit(Inference instance) ;
    
    R visit(InferenceFailure instance) ;
    
    R visit(JsonCoder instance) ;
    
    R visit(JsonDecode instance) ;
    
    R visit(JsonEncode instance) ;
    
    R visit(JsonParser instance) ;
    
    R visit(JsonRoundtrip instance) ;
    
    R visit(JsonWriter instance) ;
    
    R visit(LiftLambdaAboveLet instance) ;
    
    R visit(Serialization instance) ;
    
    R visit(SimplifyTerm instance) ;
    
    R visit(TopologicalSort instance) ;
    
    R visit(TopologicalSortBindings instance) ;
    
    R visit(TopologicalSortSCC instance) ;
    
    R visit(TypeChecking instance) ;
    
    R visit(TypeCheckingFailure instance) ;
    
    R visit(TypeReduction instance) ;
    
    R visit(NormalizeTypeVariables instance) ;
    
    R visit(FoldOverTerm instance) ;
    
    R visit(RewriteTerm instance) ;
    
    R visit(RewriteType instance) ;
    
    R visit(HoistSubterms instance) ;
    
    R visit(HoistCaseStatements instance) ;
    
    R visit(HoistLetBindings instance) ;
    
    R visit(HoistPolymorphicLetBindings instance) ;
    
    R visit(SubstInType instance) ;
    
    R visit(VariableOccursInType instance) ;
    
    R visit(UnifyTypes instance) ;
    
    R visit(JoinTypes instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TestCase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(AlphaConversion instance) {
      return otherwise(instance);
    }
    
    default R visit(CaseConversion instance) {
      return otherwise(instance);
    }
    
    default R visit(DeannotateTerm instance) {
      return otherwise(instance);
    }
    
    default R visit(DeannotateType instance) {
      return otherwise(instance);
    }
    
    default R visit(DelegatedEvaluation instance) {
      return otherwise(instance);
    }
    
    default R visit(EtaExpansion instance) {
      return otherwise(instance);
    }
    
    default R visit(FlattenLetTerms instance) {
      return otherwise(instance);
    }
    
    default R visit(FreeVariables instance) {
      return otherwise(instance);
    }
    
    default R visit(Evaluation instance) {
      return otherwise(instance);
    }
    
    default R visit(Inference instance) {
      return otherwise(instance);
    }
    
    default R visit(InferenceFailure instance) {
      return otherwise(instance);
    }
    
    default R visit(JsonCoder instance) {
      return otherwise(instance);
    }
    
    default R visit(JsonDecode instance) {
      return otherwise(instance);
    }
    
    default R visit(JsonEncode instance) {
      return otherwise(instance);
    }
    
    default R visit(JsonParser instance) {
      return otherwise(instance);
    }
    
    default R visit(JsonRoundtrip instance) {
      return otherwise(instance);
    }
    
    default R visit(JsonWriter instance) {
      return otherwise(instance);
    }
    
    default R visit(LiftLambdaAboveLet instance) {
      return otherwise(instance);
    }
    
    default R visit(Serialization instance) {
      return otherwise(instance);
    }
    
    default R visit(SimplifyTerm instance) {
      return otherwise(instance);
    }
    
    default R visit(TopologicalSort instance) {
      return otherwise(instance);
    }
    
    default R visit(TopologicalSortBindings instance) {
      return otherwise(instance);
    }
    
    default R visit(TopologicalSortSCC instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeChecking instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeCheckingFailure instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeReduction instance) {
      return otherwise(instance);
    }
    
    default R visit(NormalizeTypeVariables instance) {
      return otherwise(instance);
    }
    
    default R visit(FoldOverTerm instance) {
      return otherwise(instance);
    }
    
    default R visit(RewriteTerm instance) {
      return otherwise(instance);
    }
    
    default R visit(RewriteType instance) {
      return otherwise(instance);
    }
    
    default R visit(HoistSubterms instance) {
      return otherwise(instance);
    }
    
    default R visit(HoistCaseStatements instance) {
      return otherwise(instance);
    }
    
    default R visit(HoistLetBindings instance) {
      return otherwise(instance);
    }
    
    default R visit(HoistPolymorphicLetBindings instance) {
      return otherwise(instance);
    }
    
    default R visit(SubstInType instance) {
      return otherwise(instance);
    }
    
    default R visit(VariableOccursInType instance) {
      return otherwise(instance);
    }
    
    default R visit(UnifyTypes instance) {
      return otherwise(instance);
    }
    
    default R visit(JoinTypes instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * An alpha conversion test
   */
  public static final class AlphaConversion extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.AlphaConversionTestCase value;
    
    public AlphaConversion (hydra.testing.AlphaConversionTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AlphaConversion)) {
        return false;
      }
      AlphaConversion o = (AlphaConversion) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AlphaConversion o = (AlphaConversion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A case conversion test
   */
  public static final class CaseConversion extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.CaseConversionTestCase value;
    
    public CaseConversion (hydra.testing.CaseConversionTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseConversion)) {
        return false;
      }
      CaseConversion o = (CaseConversion) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CaseConversion o = (CaseConversion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A deannotate term test
   */
  public static final class DeannotateTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.DeannotateTermTestCase value;
    
    public DeannotateTerm (hydra.testing.DeannotateTermTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DeannotateTerm)) {
        return false;
      }
      DeannotateTerm o = (DeannotateTerm) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DeannotateTerm o = (DeannotateTerm) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A deannotate type test
   */
  public static final class DeannotateType extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.DeannotateTypeTestCase value;
    
    public DeannotateType (hydra.testing.DeannotateTypeTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DeannotateType)) {
        return false;
      }
      DeannotateType o = (DeannotateType) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DeannotateType o = (DeannotateType) other;
      return ((Comparable) value).compareTo(o.value);
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
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DelegatedEvaluation)) {
        return false;
      }
      DelegatedEvaluation o = (DelegatedEvaluation) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DelegatedEvaluation o = (DelegatedEvaluation) other;
      return ((Comparable) value).compareTo(o.value);
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
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EtaExpansion)) {
        return false;
      }
      EtaExpansion o = (EtaExpansion) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EtaExpansion o = (EtaExpansion) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A flatten let terms test
   */
  public static final class FlattenLetTerms extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.FlattenLetTermsTestCase value;
    
    public FlattenLetTerms (hydra.testing.FlattenLetTermsTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FlattenLetTerms)) {
        return false;
      }
      FlattenLetTerms o = (FlattenLetTerms) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FlattenLetTerms o = (FlattenLetTerms) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A free variables test
   */
  public static final class FreeVariables extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.FreeVariablesTestCase value;
    
    public FreeVariables (hydra.testing.FreeVariablesTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FreeVariables)) {
        return false;
      }
      FreeVariables o = (FreeVariables) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FreeVariables o = (FreeVariables) other;
      return ((Comparable) value).compareTo(o.value);
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
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Evaluation)) {
        return false;
      }
      Evaluation o = (Evaluation) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Evaluation o = (Evaluation) other;
      return ((Comparable) value).compareTo(o.value);
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
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inference)) {
        return false;
      }
      Inference o = (Inference) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Inference o = (Inference) other;
      return ((Comparable) value).compareTo(o.value);
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
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InferenceFailure)) {
        return false;
      }
      InferenceFailure o = (InferenceFailure) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InferenceFailure o = (InferenceFailure) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A JSON coder (round-trip) test using Flow-based coder
   */
  public static final class JsonCoder extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonCoderTestCase value;
    
    public JsonCoder (hydra.testing.JsonCoderTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonCoder)) {
        return false;
      }
      JsonCoder o = (JsonCoder) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      JsonCoder o = (JsonCoder) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A JSON decode test using Either-based decoder
   */
  public static final class JsonDecode extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonDecodeTestCase value;
    
    public JsonDecode (hydra.testing.JsonDecodeTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonDecode)) {
        return false;
      }
      JsonDecode o = (JsonDecode) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      JsonDecode o = (JsonDecode) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A JSON encode test using Either-based encoder
   */
  public static final class JsonEncode extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonEncodeTestCase value;
    
    public JsonEncode (hydra.testing.JsonEncodeTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonEncode)) {
        return false;
      }
      JsonEncode o = (JsonEncode) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      JsonEncode o = (JsonEncode) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A JSON parser test
   */
  public static final class JsonParser extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.ParserTestCase<hydra.json.model.Value> value;
    
    public JsonParser (hydra.testing.ParserTestCase<hydra.json.model.Value> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonParser)) {
        return false;
      }
      JsonParser o = (JsonParser) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      JsonParser o = (JsonParser) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A JSON round-trip test using Either-based encoder/decoder
   */
  public static final class JsonRoundtrip extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonRoundtripTestCase value;
    
    public JsonRoundtrip (hydra.testing.JsonRoundtripTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonRoundtrip)) {
        return false;
      }
      JsonRoundtrip o = (JsonRoundtrip) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      JsonRoundtrip o = (JsonRoundtrip) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A JSON writer test
   */
  public static final class JsonWriter extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.WriterTestCase<hydra.json.model.Value> value;
    
    public JsonWriter (hydra.testing.WriterTestCase<hydra.json.model.Value> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonWriter)) {
        return false;
      }
      JsonWriter o = (JsonWriter) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      JsonWriter o = (JsonWriter) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A lift lambda above let test
   */
  public static final class LiftLambdaAboveLet extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.LiftLambdaAboveLetTestCase value;
    
    public LiftLambdaAboveLet (hydra.testing.LiftLambdaAboveLetTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LiftLambdaAboveLet)) {
        return false;
      }
      LiftLambdaAboveLet o = (LiftLambdaAboveLet) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LiftLambdaAboveLet o = (LiftLambdaAboveLet) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An AST serialization test
   */
  public static final class Serialization extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.SerializationTestCase value;
    
    public Serialization (hydra.testing.SerializationTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Serialization)) {
        return false;
      }
      Serialization o = (Serialization) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Serialization o = (Serialization) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A simplify term test
   */
  public static final class SimplifyTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.SimplifyTermTestCase value;
    
    public SimplifyTerm (hydra.testing.SimplifyTermTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SimplifyTerm)) {
        return false;
      }
      SimplifyTerm o = (SimplifyTerm) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SimplifyTerm o = (SimplifyTerm) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A topological sort test
   */
  public static final class TopologicalSort extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TopologicalSortTestCase value;
    
    public TopologicalSort (hydra.testing.TopologicalSortTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TopologicalSort)) {
        return false;
      }
      TopologicalSort o = (TopologicalSort) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TopologicalSort o = (TopologicalSort) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A topological sort bindings test
   */
  public static final class TopologicalSortBindings extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TopologicalSortBindingsTestCase value;
    
    public TopologicalSortBindings (hydra.testing.TopologicalSortBindingsTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TopologicalSortBindings)) {
        return false;
      }
      TopologicalSortBindings o = (TopologicalSortBindings) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TopologicalSortBindings o = (TopologicalSortBindings) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A topological sort with SCC detection test
   */
  public static final class TopologicalSortSCC extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TopologicalSortSCCTestCase value;
    
    public TopologicalSortSCC (hydra.testing.TopologicalSortSCCTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TopologicalSortSCC)) {
        return false;
      }
      TopologicalSortSCC o = (TopologicalSortSCC) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TopologicalSortSCC o = (TopologicalSortSCC) other;
      return ((Comparable) value).compareTo(o.value);
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
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeChecking)) {
        return false;
      }
      TypeChecking o = (TypeChecking) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeChecking o = (TypeChecking) other;
      return ((Comparable) value).compareTo(o.value);
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
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeCheckingFailure)) {
        return false;
      }
      TypeCheckingFailure o = (TypeCheckingFailure) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeCheckingFailure o = (TypeCheckingFailure) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type reduction test
   */
  public static final class TypeReduction extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TypeReductionTestCase value;
    
    public TypeReduction (hydra.testing.TypeReductionTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeReduction)) {
        return false;
      }
      TypeReduction o = (TypeReduction) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeReduction o = (TypeReduction) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A normalize type variables test
   */
  public static final class NormalizeTypeVariables extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.NormalizeTypeVariablesTestCase value;
    
    public NormalizeTypeVariables (hydra.testing.NormalizeTypeVariablesTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NormalizeTypeVariables)) {
        return false;
      }
      NormalizeTypeVariables o = (NormalizeTypeVariables) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NormalizeTypeVariables o = (NormalizeTypeVariables) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A fold over term test
   */
  public static final class FoldOverTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.FoldOverTermTestCase value;
    
    public FoldOverTerm (hydra.testing.FoldOverTermTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FoldOverTerm)) {
        return false;
      }
      FoldOverTerm o = (FoldOverTerm) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FoldOverTerm o = (FoldOverTerm) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A rewrite term test
   */
  public static final class RewriteTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.RewriteTermTestCase value;
    
    public RewriteTerm (hydra.testing.RewriteTermTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RewriteTerm)) {
        return false;
      }
      RewriteTerm o = (RewriteTerm) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RewriteTerm o = (RewriteTerm) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A rewrite type test
   */
  public static final class RewriteType extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.RewriteTypeTestCase value;
    
    public RewriteType (hydra.testing.RewriteTypeTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RewriteType)) {
        return false;
      }
      RewriteType o = (RewriteType) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RewriteType o = (RewriteType) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A hoist subterms test
   */
  public static final class HoistSubterms extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.HoistSubtermsTestCase value;
    
    public HoistSubterms (hydra.testing.HoistSubtermsTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HoistSubterms)) {
        return false;
      }
      HoistSubterms o = (HoistSubterms) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HoistSubterms o = (HoistSubterms) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A hoist case statements test
   */
  public static final class HoistCaseStatements extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.HoistCaseStatementsTestCase value;
    
    public HoistCaseStatements (hydra.testing.HoistCaseStatementsTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HoistCaseStatements)) {
        return false;
      }
      HoistCaseStatements o = (HoistCaseStatements) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HoistCaseStatements o = (HoistCaseStatements) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A hoist all let bindings test (hoistAll=True, for Java)
   */
  public static final class HoistLetBindings extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.HoistLetBindingsTestCase value;
    
    public HoistLetBindings (hydra.testing.HoistLetBindingsTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HoistLetBindings)) {
        return false;
      }
      HoistLetBindings o = (HoistLetBindings) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HoistLetBindings o = (HoistLetBindings) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A hoist polymorphic let bindings test
   */
  public static final class HoistPolymorphicLetBindings extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.HoistPolymorphicLetBindingsTestCase value;
    
    public HoistPolymorphicLetBindings (hydra.testing.HoistPolymorphicLetBindingsTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HoistPolymorphicLetBindings)) {
        return false;
      }
      HoistPolymorphicLetBindings o = (HoistPolymorphicLetBindings) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HoistPolymorphicLetBindings o = (HoistPolymorphicLetBindings) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type substitution test
   */
  public static final class SubstInType extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.SubstInTypeTestCase value;
    
    public SubstInType (hydra.testing.SubstInTypeTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubstInType)) {
        return false;
      }
      SubstInType o = (SubstInType) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SubstInType o = (SubstInType) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An occur check test for type unification
   */
  public static final class VariableOccursInType extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.VariableOccursInTypeTestCase value;
    
    public VariableOccursInType (hydra.testing.VariableOccursInTypeTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableOccursInType)) {
        return false;
      }
      VariableOccursInType o = (VariableOccursInType) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VariableOccursInType o = (VariableOccursInType) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type unification test
   */
  public static final class UnifyTypes extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.UnifyTypesTestCase value;
    
    public UnifyTypes (hydra.testing.UnifyTypesTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnifyTypes)) {
        return false;
      }
      UnifyTypes o = (UnifyTypes) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnifyTypes o = (UnifyTypes) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A join types test (produce type constraints)
   */
  public static final class JoinTypes extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JoinTypesTestCase value;
    
    public JoinTypes (hydra.testing.JoinTypesTestCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JoinTypes)) {
        return false;
      }
      JoinTypes o = (JoinTypes) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TestCase other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      JoinTypes o = (JoinTypes) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
