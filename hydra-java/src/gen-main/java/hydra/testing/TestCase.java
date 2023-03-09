package hydra.testing;

/**
 * A simple test case with an input and an expected output
 */
public class TestCase<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/testing.TestCase");
  
  public final java.util.Optional<String> description;
  
  public final hydra.core.Term<A> input;
  
  public final hydra.core.Term<A> output;
  
  public TestCase (java.util.Optional<String> description, hydra.core.Term<A> input, hydra.core.Term<A> output) {
    this.description = description;
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TestCase)) {
      return false;
    }
    TestCase o = (TestCase) (other);
    return description.equals(o.description) && input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * input.hashCode() + 5 * output.hashCode();
  }
  
  public TestCase withDescription(java.util.Optional<String> description) {
    return new TestCase(description, input, output);
  }
  
  public TestCase withInput(hydra.core.Term<A> input) {
    return new TestCase(description, input, output);
  }
  
  public TestCase withOutput(hydra.core.Term<A> output) {
    return new TestCase(description, input, output);
  }
}