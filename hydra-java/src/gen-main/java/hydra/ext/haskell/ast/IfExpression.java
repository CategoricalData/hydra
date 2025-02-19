// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An 'if' expression
 */
public class IfExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.IfExpression");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_THEN = new hydra.core.Name("then");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final hydra.ext.haskell.ast.Expression condition;
  
  public final hydra.ext.haskell.ast.Expression then;
  
  public final hydra.ext.haskell.ast.Expression else_;
  
  public IfExpression (hydra.ext.haskell.ast.Expression condition, hydra.ext.haskell.ast.Expression then, hydra.ext.haskell.ast.Expression else_) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((then));
    java.util.Objects.requireNonNull((else_));
    this.condition = condition;
    this.then = then;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfExpression)) {
      return false;
    }
    IfExpression o = (IfExpression) (other);
    return condition.equals(o.condition) && then.equals(o.then) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * then.hashCode() + 5 * else_.hashCode();
  }
  
  public IfExpression withCondition(hydra.ext.haskell.ast.Expression condition) {
    java.util.Objects.requireNonNull((condition));
    return new IfExpression(condition, then, else_);
  }
  
  public IfExpression withThen(hydra.ext.haskell.ast.Expression then) {
    java.util.Objects.requireNonNull((then));
    return new IfExpression(condition, then, else_);
  }
  
  public IfExpression withElse(hydra.ext.haskell.ast.Expression else_) {
    java.util.Objects.requireNonNull((else_));
    return new IfExpression(condition, then, else_);
  }
}