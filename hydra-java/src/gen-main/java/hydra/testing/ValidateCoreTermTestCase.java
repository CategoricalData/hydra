// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which validates a term and compares the result with an expected Maybe InvalidTermError
 */
public class ValidateCoreTermTestCase implements Serializable, Comparable<ValidateCoreTermTestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.ValidateCoreTermTestCase");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name INPUT = new hydra.core.Name("input");

  public static final hydra.core.Name OUTPUT = new hydra.core.Name("output");

  /**
   * Whether to expect System F (typed) terms. When true, type variable binding checks and UntypedTermVariableError are active.
   */
  public final Boolean typed;

  /**
   * The term to validate
   */
  public final hydra.core.Term input;

  /**
   * The expected validation result (Nothing if valid, Just error if invalid)
   */
  public final hydra.util.Maybe<hydra.error.core.InvalidTermError> output;

  public ValidateCoreTermTestCase (Boolean typed, hydra.core.Term input, hydra.util.Maybe<hydra.error.core.InvalidTermError> output) {
    this.typed = typed;
    this.input = input;
    this.output = output;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValidateCoreTermTestCase)) {
      return false;
    }
    ValidateCoreTermTestCase o = (ValidateCoreTermTestCase) other;
    return java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.output,
      o.output);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typed) + 3 * java.util.Objects.hashCode(input) + 5 * java.util.Objects.hashCode(output);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ValidateCoreTermTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) typed).compareTo(other.typed);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }

  public ValidateCoreTermTestCase withTyped(Boolean typed) {
    return new ValidateCoreTermTestCase(typed, input, output);
  }

  public ValidateCoreTermTestCase withInput(hydra.core.Term input) {
    return new ValidateCoreTermTestCase(typed, input, output);
  }

  public ValidateCoreTermTestCase withOutput(hydra.util.Maybe<hydra.error.core.InvalidTermError> output) {
    return new ValidateCoreTermTestCase(typed, input, output);
  }
}
