// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A simple test case with an input and an expected output
 */
public class TestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/testing.TestCase");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_EVALUATION_STYLE = new hydra.core.Name("evaluationStyle");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  public final hydra.util.Opt<String> description;
  
  public final hydra.testing.EvaluationStyle evaluationStyle;
  
  public final hydra.core.Term input;
  
  public final hydra.core.Term output;
  
  public TestCase (hydra.util.Opt<String> description, hydra.testing.EvaluationStyle evaluationStyle, hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((evaluationStyle));
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.description = description;
    this.evaluationStyle = evaluationStyle;
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TestCase)) {
      return false;
    }
    TestCase o = (TestCase) (other);
    return description.equals(o.description) && evaluationStyle.equals(o.evaluationStyle) && input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * evaluationStyle.hashCode() + 5 * input.hashCode() + 7 * output.hashCode();
  }
  
  public TestCase withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new TestCase(description, evaluationStyle, input, output);
  }
  
  public TestCase withEvaluationStyle(hydra.testing.EvaluationStyle evaluationStyle) {
    java.util.Objects.requireNonNull((evaluationStyle));
    return new TestCase(description, evaluationStyle, input, output);
  }
  
  public TestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new TestCase(description, evaluationStyle, input, output);
  }
  
  public TestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new TestCase(description, evaluationStyle, input, output);
  }
}