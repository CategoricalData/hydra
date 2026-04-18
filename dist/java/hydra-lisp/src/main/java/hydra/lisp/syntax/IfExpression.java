// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * Conditional: (if test then else)
 */
public class IfExpression implements Serializable, Comparable<IfExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.IfExpression");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public static final hydra.core.Name THEN = new hydra.core.Name("then");

  public static final hydra.core.Name ELSE = new hydra.core.Name("else");

  /**
   * The test expression
   */
  public final hydra.lisp.syntax.Expression condition;

  /**
   * The then branch
   */
  public final hydra.lisp.syntax.Expression then;

  /**
   * Optional else branch
   */
  public final hydra.util.Maybe<hydra.lisp.syntax.Expression> else_;

  public IfExpression (hydra.lisp.syntax.Expression condition, hydra.lisp.syntax.Expression then, hydra.util.Maybe<hydra.lisp.syntax.Expression> else_) {
    this.condition = condition;
    this.then = then;
    this.else_ = else_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfExpression)) {
      return false;
    }
    IfExpression o = (IfExpression) other;
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
    cmp = hydra.util.Comparing.compare(
      condition,
      other.condition);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      then,
      other.then);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      else_,
      other.else_);
  }

  public IfExpression withCondition(hydra.lisp.syntax.Expression condition) {
    return new IfExpression(condition, then, else_);
  }

  public IfExpression withThen(hydra.lisp.syntax.Expression then) {
    return new IfExpression(condition, then, else_);
  }

  public IfExpression withElse(hydra.util.Maybe<hydra.lisp.syntax.Expression> else_) {
    return new IfExpression(condition, then, else_);
  }
}
