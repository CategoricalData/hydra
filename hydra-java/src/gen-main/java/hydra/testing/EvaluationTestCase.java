// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which evaluates (reduces) a given term and compares it with the expected result
 */
public class EvaluationTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/testing.EvaluationTestCase");
  
  public static final hydra.core.Name FIELD_NAME_EVALUATION_STYLE = new hydra.core.Name("evaluationStyle");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  public final hydra.testing.EvaluationStyle evaluationStyle;
  
  public final hydra.core.Term input;
  
  public final hydra.core.Term output;
  
  public EvaluationTestCase (hydra.testing.EvaluationStyle evaluationStyle, hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((evaluationStyle));
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.evaluationStyle = evaluationStyle;
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EvaluationTestCase)) {
      return false;
    }
    EvaluationTestCase o = (EvaluationTestCase) (other);
    return evaluationStyle.equals(o.evaluationStyle) && input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * evaluationStyle.hashCode() + 3 * input.hashCode() + 5 * output.hashCode();
  }
  
  public EvaluationTestCase withEvaluationStyle(hydra.testing.EvaluationStyle evaluationStyle) {
    java.util.Objects.requireNonNull((evaluationStyle));
    return new EvaluationTestCase(evaluationStyle, input, output);
  }
  
  public EvaluationTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new EvaluationTestCase(evaluationStyle, input, output);
  }
  
  public EvaluationTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new EvaluationTestCase(evaluationStyle, input, output);
  }
}