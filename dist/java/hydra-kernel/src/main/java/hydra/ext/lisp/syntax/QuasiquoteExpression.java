// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A quasiquoted form: `expr
 */
public class QuasiquoteExpression implements Serializable, Comparable<QuasiquoteExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.QuasiquoteExpression");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The quasiquoted form
   */
  public final hydra.ext.lisp.syntax.Expression body;

  public QuasiquoteExpression (hydra.ext.lisp.syntax.Expression body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QuasiquoteExpression)) {
      return false;
    }
    QuasiquoteExpression o = (QuasiquoteExpression) other;
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
  public int compareTo(QuasiquoteExpression other) {
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }
}
