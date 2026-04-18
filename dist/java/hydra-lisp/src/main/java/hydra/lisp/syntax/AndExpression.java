// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * Logical and: (and expr1 expr2 ...)
 */
public class AndExpression implements Serializable, Comparable<AndExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.AndExpression");

  public static final hydra.core.Name EXPRESSIONS = new hydra.core.Name("expressions");

  /**
   * The operand expressions
   */
  public final java.util.List<hydra.lisp.syntax.Expression> expressions;

  public AndExpression (java.util.List<hydra.lisp.syntax.Expression> expressions) {
    this.expressions = expressions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AndExpression)) {
      return false;
    }
    AndExpression o = (AndExpression) other;
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
  public int compareTo(AndExpression other) {
    return hydra.util.Comparing.compare(
      expressions,
      other.expressions);
  }
}
