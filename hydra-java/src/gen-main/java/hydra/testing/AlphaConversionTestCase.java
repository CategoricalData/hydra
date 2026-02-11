// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs alpha conversion (variable renaming) on a term and compares the result with the expected term
 */
public class AlphaConversionTestCase implements Serializable, Comparable<AlphaConversionTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.AlphaConversionTestCase");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_OLD_VARIABLE = new hydra.core.Name("oldVariable");
  
  public static final hydra.core.Name FIELD_NAME_NEW_VARIABLE = new hydra.core.Name("newVariable");
  
  public static final hydra.core.Name FIELD_NAME_RESULT = new hydra.core.Name("result");
  
  /**
   * The term on which to perform alpha conversion
   */
  public final hydra.core.Term term;
  
  /**
   * The variable name to replace
   */
  public final hydra.core.Name oldVariable;
  
  /**
   * The new variable name
   */
  public final hydra.core.Name newVariable;
  
  /**
   * The expected result term after alpha conversion
   */
  public final hydra.core.Term result;
  
  public AlphaConversionTestCase (hydra.core.Term term, hydra.core.Name oldVariable, hydra.core.Name newVariable, hydra.core.Term result) {
    this.term = term;
    this.oldVariable = oldVariable;
    this.newVariable = newVariable;
    this.result = result;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AlphaConversionTestCase)) {
      return false;
    }
    AlphaConversionTestCase o = (AlphaConversionTestCase) other;
    return java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.oldVariable,
      o.oldVariable) && java.util.Objects.equals(
      this.newVariable,
      o.newVariable) && java.util.Objects.equals(
      this.result,
      o.result);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(term) + 3 * java.util.Objects.hashCode(oldVariable) + 5 * java.util.Objects.hashCode(newVariable) + 7 * java.util.Objects.hashCode(result);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AlphaConversionTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) term).compareTo(other.term);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) oldVariable).compareTo(other.oldVariable);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) newVariable).compareTo(other.newVariable);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) result).compareTo(other.result);
  }
  
  public AlphaConversionTestCase withTerm(hydra.core.Term term) {
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
  
  public AlphaConversionTestCase withOldVariable(hydra.core.Name oldVariable) {
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
  
  public AlphaConversionTestCase withNewVariable(hydra.core.Name newVariable) {
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
  
  public AlphaConversionTestCase withResult(hydra.core.Term result) {
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
}
