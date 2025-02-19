// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class Ordering implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Ordering");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTION = new hydra.core.Name("direction");
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.OrderingDirection> direction;
  
  public Ordering (hydra.ext.csharp.syntax.Expression expression, hydra.util.Opt<hydra.ext.csharp.syntax.OrderingDirection> direction) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((direction));
    this.expression = expression;
    this.direction = direction;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ordering)) {
      return false;
    }
    Ordering o = (Ordering) (other);
    return expression.equals(o.expression) && direction.equals(o.direction);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * direction.hashCode();
  }
  
  public Ordering withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new Ordering(expression, direction);
  }
  
  public Ordering withDirection(hydra.util.Opt<hydra.ext.csharp.syntax.OrderingDirection> direction) {
    java.util.Objects.requireNonNull((direction));
    return new Ordering(expression, direction);
  }
}