// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class AssignmentExpression implements Serializable, Comparable<AssignmentExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.AssignmentExpression");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.python.syntax.Name name;

  public final hydra.python.syntax.Expression expression;

  public AssignmentExpression (hydra.python.syntax.Name name, hydra.python.syntax.Expression expression) {
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
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public AssignmentExpression withName(hydra.python.syntax.Name name) {
    return new AssignmentExpression(name, expression);
  }

  public AssignmentExpression withExpression(hydra.python.syntax.Expression expression) {
    return new AssignmentExpression(name, expression);
  }
}
