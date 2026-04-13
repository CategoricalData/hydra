// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ExceptExpression implements Serializable, Comparable<ExceptExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.ExceptExpression");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public final hydra.ext.python.syntax.Expression expression;

  public final hydra.util.Maybe<hydra.ext.python.syntax.Name> as;

  public ExceptExpression (hydra.ext.python.syntax.Expression expression, hydra.util.Maybe<hydra.ext.python.syntax.Name> as) {
    this.expression = expression;
    this.as = as;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptExpression)) {
      return false;
    }
    ExceptExpression o = (ExceptExpression) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.as,
      o.as);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(as);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExceptExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      as,
      other.as);
  }

  public ExceptExpression withExpression(hydra.ext.python.syntax.Expression expression) {
    return new ExceptExpression(expression, as);
  }

  public ExceptExpression withAs(hydra.util.Maybe<hydra.ext.python.syntax.Name> as) {
    return new ExceptExpression(expression, as);
  }
}
