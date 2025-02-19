// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ExceptExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ExceptExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Name> as;
  
  public ExceptExpression (hydra.ext.python.syntax.Expression expression, hydra.util.Opt<hydra.ext.python.syntax.Name> as) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((as));
    this.expression = expression;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptExpression)) {
      return false;
    }
    ExceptExpression o = (ExceptExpression) (other);
    return expression.equals(o.expression) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * as.hashCode();
  }
  
  public ExceptExpression withExpression(hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ExceptExpression(expression, as);
  }
  
  public ExceptExpression withAs(hydra.util.Opt<hydra.ext.python.syntax.Name> as) {
    java.util.Objects.requireNonNull((as));
    return new ExceptExpression(expression, as);
  }
}