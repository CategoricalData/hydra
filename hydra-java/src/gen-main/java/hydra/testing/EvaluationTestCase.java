// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which evaluates (reduces) a given term and compares it with the expected result
 */
public class EvaluationTestCase implements Serializable, Comparable<EvaluationTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.EvaluationTestCase");
  
  public static final hydra.core.Name FIELD_NAME_EVALUATION_STYLE = new hydra.core.Name("evaluationStyle");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The evaluation style (eager or lazy)
   */
  public final hydra.testing.EvaluationStyle evaluationStyle;
  
  /**
   * The term to evaluate
   */
  public final hydra.core.Term input;
  
  /**
   * The expected result
   */
  public final hydra.core.Term output;
  
  public EvaluationTestCase (hydra.testing.EvaluationStyle evaluationStyle, hydra.core.Term input, hydra.core.Term output) {
    this.evaluationStyle = evaluationStyle;
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EvaluationTestCase)) {
      return false;
    }
    EvaluationTestCase o = (EvaluationTestCase) other;
    return java.util.Objects.equals(
      this.evaluationStyle,
      o.evaluationStyle) && java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.output,
      o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(evaluationStyle) + 3 * java.util.Objects.hashCode(input) + 5 * java.util.Objects.hashCode(output);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EvaluationTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) evaluationStyle).compareTo(other.evaluationStyle);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public EvaluationTestCase withEvaluationStyle(hydra.testing.EvaluationStyle evaluationStyle) {
    return new EvaluationTestCase(evaluationStyle, input, output);
  }
  
  public EvaluationTestCase withInput(hydra.core.Term input) {
    return new EvaluationTestCase(evaluationStyle, input, output);
  }
  
  public EvaluationTestCase withOutput(hydra.core.Term output) {
    return new EvaluationTestCase(evaluationStyle, input, output);
  }
}
