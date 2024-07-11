// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A simple test case with an input and an expected output
 */
public class TestCase<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/testing.TestCase");
  
  public final hydra.util.Opt<String> description;
  
  public final hydra.testing.EvaluationStyle evaluationStyle;
  
  public final hydra.core.Term<A> input;
  
  public final hydra.core.Term<A> output;
  
  public TestCase (hydra.util.Opt<String> description, hydra.testing.EvaluationStyle evaluationStyle, hydra.core.Term<A> input, hydra.core.Term<A> output) {
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
  
  public TestCase withInput(hydra.core.Term<A> input) {
    java.util.Objects.requireNonNull((input));
    return new TestCase(description, evaluationStyle, input, output);
  }
  
  public TestCase withOutput(hydra.core.Term<A> output) {
    java.util.Objects.requireNonNull((output));
    return new TestCase(description, evaluationStyle, input, output);
  }
}