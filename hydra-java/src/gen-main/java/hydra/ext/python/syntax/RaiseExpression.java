// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class RaiseExpression implements Serializable, Comparable<RaiseExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.RaiseExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_FROM = new hydra.core.Name("from");
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> from;
  
  public RaiseExpression (hydra.ext.python.syntax.Expression expression, hydra.util.Maybe<hydra.ext.python.syntax.Expression> from) {
    this.expression = expression;
    this.from = from;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RaiseExpression)) {
      return false;
    }
    RaiseExpression o = (RaiseExpression) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.from,
      o.from);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(from);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RaiseExpression other) {
    int cmp = 0;
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      from.hashCode(),
      other.from.hashCode());
  }
  
  public RaiseExpression withExpression(hydra.ext.python.syntax.Expression expression) {
    return new RaiseExpression(expression, from);
  }
  
  public RaiseExpression withFrom(hydra.util.Maybe<hydra.ext.python.syntax.Expression> from) {
    return new RaiseExpression(expression, from);
  }
}
