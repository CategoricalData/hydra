// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which checks whether a type variable occurs in a type expression. This is the occur check used in type unification.
 */
public class VariableOccursInTypeTestCase implements Serializable, Comparable<VariableOccursInTypeTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The variable name to search for
   */
  public final hydra.core.Name variable;
  
  /**
   * The type to search within
   */
  public final hydra.core.Type type;
  
  /**
   * Whether the variable occurs in the type
   */
  public final Boolean expected;
  
  public VariableOccursInTypeTestCase (hydra.core.Name variable, hydra.core.Type type, Boolean expected) {
    this.variable = variable;
    this.type = type;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableOccursInTypeTestCase)) {
      return false;
    }
    VariableOccursInTypeTestCase o = (VariableOccursInTypeTestCase) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.expected,
      o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(expected);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableOccursInTypeTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) variable).compareTo(other.variable);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expected).compareTo(other.expected);
  }
  
  public VariableOccursInTypeTestCase withVariable(hydra.core.Name variable) {
    return new VariableOccursInTypeTestCase(variable, type, expected);
  }
  
  public VariableOccursInTypeTestCase withType(hydra.core.Type type) {
    return new VariableOccursInTypeTestCase(variable, type, expected);
  }
  
  public VariableOccursInTypeTestCase withExpected(Boolean expected) {
    return new VariableOccursInTypeTestCase(variable, type, expected);
  }
}
