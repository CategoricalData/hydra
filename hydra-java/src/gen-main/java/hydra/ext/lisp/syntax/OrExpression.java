// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * Logical or: (or expr1 expr2 ...)
 */
public class OrExpression implements Serializable, Comparable<OrExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.OrExpression");

  public static final hydra.core.Name EXPRESSIONS = new hydra.core.Name("expressions");

  /**
   * The operand expressions
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> expressions;

  public OrExpression (hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> expressions) {
    this.expressions = expressions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrExpression)) {
      return false;
    }
    OrExpression o = (OrExpression) other;
    return java.util.Objects.equals(
      this.expressions,
      o.expressions);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expressions);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OrExpression other) {
    return ((Comparable) expressions).compareTo(other.expressions);
  }
}
