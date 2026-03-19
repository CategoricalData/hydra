// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * An explicit begin block (distinct from do for Scheme compatibility)
 */
public class BeginExpression implements Serializable, Comparable<BeginExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.BeginExpression");

  public static final hydra.core.Name EXPRESSIONS = new hydra.core.Name("expressions");

  /**
   * The expressions to evaluate in sequence
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> expressions;

  public BeginExpression (hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> expressions) {
    this.expressions = expressions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BeginExpression)) {
      return false;
    }
    BeginExpression o = (BeginExpression) other;
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
  public int compareTo(BeginExpression other) {
    return ((Comparable) expressions).compareTo(other.expressions);
  }
}
