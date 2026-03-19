// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A clause in a cond expression
 */
public class CondClause implements Serializable, Comparable<CondClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.CondClause");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The test condition
   */
  public final hydra.ext.lisp.syntax.Expression condition;

  /**
   * The result expression
   */
  public final hydra.ext.lisp.syntax.Expression body;

  public CondClause (hydra.ext.lisp.syntax.Expression condition, hydra.ext.lisp.syntax.Expression body) {
    this.condition = condition;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CondClause)) {
      return false;
    }
    CondClause o = (CondClause) other;
    return java.util.Objects.equals(
      this.condition,
      o.condition) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CondClause other) {
    int cmp = 0;
    cmp = ((Comparable) condition).compareTo(other.condition);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public CondClause withCondition(hydra.ext.lisp.syntax.Expression condition) {
    return new CondClause(condition, body);
  }

  public CondClause withBody(hydra.ext.lisp.syntax.Expression body) {
    return new CondClause(condition, body);
  }
}
