// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs alpha conversion (variable renaming) on a term and compares the result with the expected term
 */
public class AlphaConversionTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((oldVariable));
    java.util.Objects.requireNonNull((newVariable));
    java.util.Objects.requireNonNull((result));
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
    AlphaConversionTestCase o = (AlphaConversionTestCase) (other);
    return term.equals(o.term) && oldVariable.equals(o.oldVariable) && newVariable.equals(o.newVariable) && result.equals(o.result);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * oldVariable.hashCode() + 5 * newVariable.hashCode() + 7 * result.hashCode();
  }
  
  public AlphaConversionTestCase withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
  
  public AlphaConversionTestCase withOldVariable(hydra.core.Name oldVariable) {
    java.util.Objects.requireNonNull((oldVariable));
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
  
  public AlphaConversionTestCase withNewVariable(hydra.core.Name newVariable) {
    java.util.Objects.requireNonNull((newVariable));
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
  
  public AlphaConversionTestCase withResult(hydra.core.Term result) {
    java.util.Objects.requireNonNull((result));
    return new AlphaConversionTestCase(term, oldVariable, newVariable, result);
  }
}
