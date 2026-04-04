// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * An unquoted form within a quasiquote: ~expr or ,expr
 */
public class UnquoteExpression implements Serializable, Comparable<UnquoteExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.UnquoteExpression");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The unquoted form
   */
  public final hydra.ext.lisp.syntax.Expression body;

  public UnquoteExpression (hydra.ext.lisp.syntax.Expression body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnquoteExpression)) {
      return false;
    }
    UnquoteExpression o = (UnquoteExpression) other;
    return java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnquoteExpression other) {
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }
}
