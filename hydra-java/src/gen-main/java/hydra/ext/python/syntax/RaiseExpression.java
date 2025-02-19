// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class RaiseExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.RaiseExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_FROM = new hydra.core.Name("from");
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> from;
  
  public RaiseExpression (hydra.ext.python.syntax.Expression expression, hydra.util.Opt<hydra.ext.python.syntax.Expression> from) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((from));
    this.expression = expression;
    this.from = from;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RaiseExpression)) {
      return false;
    }
    RaiseExpression o = (RaiseExpression) (other);
    return expression.equals(o.expression) && from.equals(o.from);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * from.hashCode();
  }
  
  public RaiseExpression withExpression(hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new RaiseExpression(expression, from);
  }
  
  public RaiseExpression withFrom(hydra.util.Opt<hydra.ext.python.syntax.Expression> from) {
    java.util.Objects.requireNonNull((from));
    return new RaiseExpression(expression, from);
  }
}