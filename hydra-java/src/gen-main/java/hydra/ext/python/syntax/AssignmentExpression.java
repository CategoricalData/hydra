// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AssignmentExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AssignmentExpression");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public AssignmentExpression (hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((expression));
    this.name = name;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssignmentExpression)) {
      return false;
    }
    AssignmentExpression o = (AssignmentExpression) (other);
    return name.equals(o.name) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * expression.hashCode();
  }
  
  public AssignmentExpression withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new AssignmentExpression(name, expression);
  }
  
  public AssignmentExpression withExpression(hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new AssignmentExpression(name, expression);
  }
}