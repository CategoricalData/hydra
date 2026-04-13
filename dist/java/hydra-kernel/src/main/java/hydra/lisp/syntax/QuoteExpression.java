// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A quoted form: 'expr or (quote expr)
 */
public class QuoteExpression implements Serializable, Comparable<QuoteExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.QuoteExpression");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The quoted form
   */
  public final hydra.lisp.syntax.Expression body;

  public QuoteExpression (hydra.lisp.syntax.Expression body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QuoteExpression)) {
      return false;
    }
    QuoteExpression o = (QuoteExpression) other;
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
  public int compareTo(QuoteExpression other) {
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }
}
