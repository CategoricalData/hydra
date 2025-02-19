// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class WithItem implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.WithItem");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.StarTarget> as;
  
  public WithItem (hydra.ext.python.syntax.Expression expression, hydra.util.Opt<hydra.ext.python.syntax.StarTarget> as) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((as));
    this.expression = expression;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WithItem)) {
      return false;
    }
    WithItem o = (WithItem) (other);
    return expression.equals(o.expression) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * as.hashCode();
  }
  
  public WithItem withExpression(hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new WithItem(expression, as);
  }
  
  public WithItem withAs(hydra.util.Opt<hydra.ext.python.syntax.StarTarget> as) {
    java.util.Objects.requireNonNull((as));
    return new WithItem(expression, as);
  }
}