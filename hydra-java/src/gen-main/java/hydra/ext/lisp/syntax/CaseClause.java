// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A clause in a case expression
 */
public class CaseClause implements Serializable, Comparable<CaseClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.CaseClause");

  public static final hydra.core.Name KEYS = new hydra.core.Name("keys");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The matching keys (one or more datum values)
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> keys;

  /**
   * The result expression
   */
  public final hydra.ext.lisp.syntax.Expression body;

  public CaseClause (hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> keys, hydra.ext.lisp.syntax.Expression body) {
    this.keys = keys;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseClause)) {
      return false;
    }
    CaseClause o = (CaseClause) other;
    return java.util.Objects.equals(
      this.keys,
      o.keys) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(keys) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseClause other) {
    int cmp = 0;
    cmp = ((Comparable) keys).compareTo(other.keys);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public CaseClause withKeys(hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> keys) {
    return new CaseClause(keys, body);
  }

  public CaseClause withBody(hydra.ext.lisp.syntax.Expression body) {
    return new CaseClause(keys, body);
  }
}
