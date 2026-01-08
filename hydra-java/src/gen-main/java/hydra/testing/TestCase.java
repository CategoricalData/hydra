// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A simple test case with an input and an expected output
 */
public abstract class TestCase implements Serializable {
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
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TestCase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AlphaConversion instance) {
      return otherwise((instance));
    }
    
    default R visit(CaseConversion instance) {
      return otherwise((instance));
    }
    
    default R visit(DeannotateTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(DeannotateType instance) {
      return otherwise((instance));
    }
    
    default R visit(DelegatedEvaluation instance) {
      return otherwise((instance));
    }
    
    default R visit(EtaExpansion instance) {
      return otherwise((instance));
    }
    
    default R visit(FlattenLetTerms instance) {
      return otherwise((instance));
    }
    
    default R visit(FreeVariables instance) {
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
    
    default R visit(JsonCoder instance) {
      return otherwise((instance));
    }
    
    default R visit(JsonDecode instance) {
      return otherwise((instance));
    }
    
    default R visit(JsonEncode instance) {
      return otherwise((instance));
    }
    
    default R visit(JsonParser instance) {
      return otherwise((instance));
    }
    
    default R visit(JsonRoundtrip instance) {
      return otherwise((instance));
    }
    
    default R visit(JsonWriter instance) {
      return otherwise((instance));
    }
    
    default R visit(LiftLambdaAboveLet instance) {
      return otherwise((instance));
    }
    
    default R visit(Serialization instance) {
      return otherwise((instance));
    }
    
    default R visit(SimplifyTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(TopologicalSort instance) {
      return otherwise((instance));
    }
    
    default R visit(TopologicalSortBindings instance) {
      return otherwise((instance));
    }
    
    default R visit(TopologicalSortSCC instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeChecking instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeCheckingFailure instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeReduction instance) {
      return otherwise((instance));
    }
    
    default R visit(NormalizeTypeVariables instance) {
      return otherwise((instance));
    }
    
    default R visit(FoldOverTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(RewriteTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(RewriteType instance) {
      return otherwise((instance));
    }
    
    default R visit(HoistSubterms instance) {
      return otherwise((instance));
    }
    
    default R visit(HoistCaseStatements instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * An alpha conversion test
   */
  public static final class AlphaConversion extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.AlphaConversionTestCase value;
    
    public AlphaConversion (hydra.testing.AlphaConversionTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AlphaConversion)) {
        return false;
      }
      AlphaConversion o = (AlphaConversion) (other);
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
   * A deannotate term test
   */
  public static final class DeannotateTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.DeannotateTermTestCase value;
    
    public DeannotateTerm (hydra.testing.DeannotateTermTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DeannotateTerm)) {
        return false;
      }
      DeannotateTerm o = (DeannotateTerm) (other);
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
   * A deannotate type test
   */
  public static final class DeannotateType extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.DeannotateTypeTestCase value;
    
    public DeannotateType (hydra.testing.DeannotateTypeTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DeannotateType)) {
        return false;
      }
      DeannotateType o = (DeannotateType) (other);
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
   * A flatten let terms test
   */
  public static final class FlattenLetTerms extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.FlattenLetTermsTestCase value;
    
    public FlattenLetTerms (hydra.testing.FlattenLetTermsTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FlattenLetTerms)) {
        return false;
      }
      FlattenLetTerms o = (FlattenLetTerms) (other);
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
   * A free variables test
   */
  public static final class FreeVariables extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.FreeVariablesTestCase value;
    
    public FreeVariables (hydra.testing.FreeVariablesTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FreeVariables)) {
        return false;
      }
      FreeVariables o = (FreeVariables) (other);
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
   * A JSON coder (round-trip) test using Flow-based coder
   */
  public static final class JsonCoder extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonCoderTestCase value;
    
    public JsonCoder (hydra.testing.JsonCoderTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonCoder)) {
        return false;
      }
      JsonCoder o = (JsonCoder) (other);
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
   * A JSON decode test using Either-based decoder
   */
  public static final class JsonDecode extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonDecodeTestCase value;
    
    public JsonDecode (hydra.testing.JsonDecodeTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonDecode)) {
        return false;
      }
      JsonDecode o = (JsonDecode) (other);
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
   * A JSON encode test using Either-based encoder
   */
  public static final class JsonEncode extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonEncodeTestCase value;
    
    public JsonEncode (hydra.testing.JsonEncodeTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonEncode)) {
        return false;
      }
      JsonEncode o = (JsonEncode) (other);
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
   * A JSON parser test
   */
  public static final class JsonParser extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonParserTestCase value;
    
    public JsonParser (hydra.testing.JsonParserTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonParser)) {
        return false;
      }
      JsonParser o = (JsonParser) (other);
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
   * A JSON round-trip test using Either-based encoder/decoder
   */
  public static final class JsonRoundtrip extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonRoundtripTestCase value;
    
    public JsonRoundtrip (hydra.testing.JsonRoundtripTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonRoundtrip)) {
        return false;
      }
      JsonRoundtrip o = (JsonRoundtrip) (other);
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
   * A JSON writer test
   */
  public static final class JsonWriter extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.JsonWriterTestCase value;
    
    public JsonWriter (hydra.testing.JsonWriterTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonWriter)) {
        return false;
      }
      JsonWriter o = (JsonWriter) (other);
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
   * A lift lambda above let test
   */
  public static final class LiftLambdaAboveLet extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.LiftLambdaAboveLetTestCase value;
    
    public LiftLambdaAboveLet (hydra.testing.LiftLambdaAboveLetTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LiftLambdaAboveLet)) {
        return false;
      }
      LiftLambdaAboveLet o = (LiftLambdaAboveLet) (other);
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
   * An AST serialization test
   */
  public static final class Serialization extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.SerializationTestCase value;
    
    public Serialization (hydra.testing.SerializationTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Serialization)) {
        return false;
      }
      Serialization o = (Serialization) (other);
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
   * A simplify term test
   */
  public static final class SimplifyTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.SimplifyTermTestCase value;
    
    public SimplifyTerm (hydra.testing.SimplifyTermTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SimplifyTerm)) {
        return false;
      }
      SimplifyTerm o = (SimplifyTerm) (other);
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
   * A topological sort test
   */
  public static final class TopologicalSort extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TopologicalSortTestCase value;
    
    public TopologicalSort (hydra.testing.TopologicalSortTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TopologicalSort)) {
        return false;
      }
      TopologicalSort o = (TopologicalSort) (other);
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
   * A topological sort bindings test
   */
  public static final class TopologicalSortBindings extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TopologicalSortBindingsTestCase value;
    
    public TopologicalSortBindings (hydra.testing.TopologicalSortBindingsTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TopologicalSortBindings)) {
        return false;
      }
      TopologicalSortBindings o = (TopologicalSortBindings) (other);
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
   * A topological sort with SCC detection test
   */
  public static final class TopologicalSortSCC extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TopologicalSortSCCTestCase value;
    
    public TopologicalSortSCC (hydra.testing.TopologicalSortSCCTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TopologicalSortSCC)) {
        return false;
      }
      TopologicalSortSCC o = (TopologicalSortSCC) (other);
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
  
  /**
   * A type reduction test
   */
  public static final class TypeReduction extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.TypeReductionTestCase value;
    
    public TypeReduction (hydra.testing.TypeReductionTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeReduction)) {
        return false;
      }
      TypeReduction o = (TypeReduction) (other);
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
   * A normalize type variables test
   */
  public static final class NormalizeTypeVariables extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.NormalizeTypeVariablesTestCase value;
    
    public NormalizeTypeVariables (hydra.testing.NormalizeTypeVariablesTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NormalizeTypeVariables)) {
        return false;
      }
      NormalizeTypeVariables o = (NormalizeTypeVariables) (other);
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
   * A fold over term test
   */
  public static final class FoldOverTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.FoldOverTermTestCase value;
    
    public FoldOverTerm (hydra.testing.FoldOverTermTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FoldOverTerm)) {
        return false;
      }
      FoldOverTerm o = (FoldOverTerm) (other);
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
   * A rewrite term test
   */
  public static final class RewriteTerm extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.RewriteTermTestCase value;
    
    public RewriteTerm (hydra.testing.RewriteTermTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RewriteTerm)) {
        return false;
      }
      RewriteTerm o = (RewriteTerm) (other);
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
   * A rewrite type test
   */
  public static final class RewriteType extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.RewriteTypeTestCase value;
    
    public RewriteType (hydra.testing.RewriteTypeTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RewriteType)) {
        return false;
      }
      RewriteType o = (RewriteType) (other);
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
   * A hoist subterms test
   */
  public static final class HoistSubterms extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.HoistSubtermsTestCase value;
    
    public HoistSubterms (hydra.testing.HoistSubtermsTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HoistSubterms)) {
        return false;
      }
      HoistSubterms o = (HoistSubterms) (other);
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
   * A hoist case statements test
   */
  public static final class HoistCaseStatements extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.HoistCaseStatementsTestCase value;
    
    public HoistCaseStatements (hydra.testing.HoistCaseStatementsTestCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HoistCaseStatements)) {
        return false;
      }
      HoistCaseStatements o = (HoistCaseStatements) (other);
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
