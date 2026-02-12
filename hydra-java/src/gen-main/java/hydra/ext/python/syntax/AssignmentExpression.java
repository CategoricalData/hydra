// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AssignmentExpression implements Serializable, Comparable<AssignmentExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AssignmentExpression");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public AssignmentExpression (hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.Expression expression) {
    this.name = name;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssignmentExpression)) {
      return false;
    }
    AssignmentExpression o = (AssignmentExpression) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(expression);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AssignmentExpression other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }
  
  public AssignmentExpression withName(hydra.ext.python.syntax.Name name) {
    return new AssignmentExpression(name, expression);
  }
  
  public AssignmentExpression withExpression(hydra.ext.python.syntax.Expression expression) {
    return new AssignmentExpression(name, expression);
  }
}
