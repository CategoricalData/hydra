// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A splicing unquote within a quasiquote: ~@expr or ,@expr
 */
public class SplicingUnquoteExpression implements Serializable, Comparable<SplicingUnquoteExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.SplicingUnquoteExpression");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The spliced form
   */
  public final hydra.ext.lisp.syntax.Expression body;

  public SplicingUnquoteExpression (hydra.ext.lisp.syntax.Expression body) {
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SplicingUnquoteExpression)) {
      return false;
    }
    SplicingUnquoteExpression o = (SplicingUnquoteExpression) other;
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
  public int compareTo(SplicingUnquoteExpression other) {
    return ((Comparable) body).compareTo(other.body);
  }
}
