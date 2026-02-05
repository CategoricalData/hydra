// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An 'if' expression
 */
public class IfExpression implements Serializable, Comparable<IfExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.IfExpression");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_THEN = new hydra.core.Name("then");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  /**
   * The condition expression
   */
  public final hydra.ext.haskell.ast.Expression condition;
  
  /**
   * The 'then' branch
   */
  public final hydra.ext.haskell.ast.Expression then;
  
  /**
   * The 'else' branch
   */
  public final hydra.ext.haskell.ast.Expression else_;
  
  public IfExpression (hydra.ext.haskell.ast.Expression condition, hydra.ext.haskell.ast.Expression then, hydra.ext.haskell.ast.Expression else_) {
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
    return java.util.Objects.equals(
      this.condition,
      o.condition) && java.util.Objects.equals(
      this.then,
      o.then) && java.util.Objects.equals(
      this.else_,
      o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition) + 3 * java.util.Objects.hashCode(then) + 5 * java.util.Objects.hashCode(else_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IfExpression other) {
    int cmp = 0;
    cmp = ((Comparable) (condition)).compareTo(other.condition);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (then)).compareTo(other.then);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (else_)).compareTo(other.else_);
  }
  
  public IfExpression withCondition(hydra.ext.haskell.ast.Expression condition) {
    return new IfExpression(condition, then, else_);
  }
  
  public IfExpression withThen(hydra.ext.haskell.ast.Expression then) {
    return new IfExpression(condition, then, else_);
  }
  
  public IfExpression withElse(hydra.ext.haskell.ast.Expression else_) {
    return new IfExpression(condition, then, else_);
  }
}
