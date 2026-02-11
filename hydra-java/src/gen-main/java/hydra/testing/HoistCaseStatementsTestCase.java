// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for the hoistCaseStatements function, which hoists case statements into let bindings, but only when they appear inside a lambda body. This is used for targets like Python which don't support inline match expressions.
 */
public class HoistCaseStatementsTestCase implements Serializable, Comparable<HoistCaseStatementsTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The input term
   */
  public final hydra.core.Term input;
  
  /**
   * The expected output term with hoisted case statements
   */
  public final hydra.core.Term output;
  
  public HoistCaseStatementsTestCase (hydra.core.Term input, hydra.core.Term output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HoistCaseStatementsTestCase)) {
      return false;
    }
    HoistCaseStatementsTestCase o = (HoistCaseStatementsTestCase) other;
    return java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.output,
      o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(input) + 3 * java.util.Objects.hashCode(output);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(HoistCaseStatementsTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public HoistCaseStatementsTestCase withInput(hydra.core.Term input) {
    return new HoistCaseStatementsTestCase(input, output);
  }
  
  public HoistCaseStatementsTestCase withOutput(hydra.core.Term output) {
    return new HoistCaseStatementsTestCase(input, output);
  }
}
