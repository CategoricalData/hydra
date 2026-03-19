// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * Sequential evaluation of expressions, returning the last. Serializes as (do expr1 expr2 ...) in Clojure, (progn expr1 expr2 ...) in Emacs Lisp and Common Lisp, (begin expr1 expr2 ...) in Scheme
 */
public class DoExpression implements Serializable, Comparable<DoExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.DoExpression");

  public static final hydra.core.Name EXPRESSIONS = new hydra.core.Name("expressions");

  /**
   * The expressions to evaluate in sequence
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> expressions;

  public DoExpression (hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> expressions) {
    this.expressions = expressions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DoExpression)) {
      return false;
    }
    DoExpression o = (DoExpression) other;
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
  public int compareTo(DoExpression other) {
    return ((Comparable) expressions).compareTo(other.expressions);
  }
}
