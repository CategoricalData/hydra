// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * Multi-branch conditional. Serializes as (cond test1 expr1 test2 expr2 :else default) in Clojure, (cond (test1 expr1) (test2 expr2) (t default)) in Emacs Lisp and Common Lisp, (cond (test1 expr1) (test2 expr2) (else default)) in Scheme
 */
public class CondExpression implements Serializable, Comparable<CondExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.CondExpression");

  public static final hydra.core.Name CLAUSES = new hydra.core.Name("clauses");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  /**
   * The condition-expression pairs
   */
  public final java.util.List<hydra.lisp.syntax.CondClause> clauses;

  /**
   * Optional default expression
   */
  public final hydra.util.Maybe<hydra.lisp.syntax.Expression> default_;

  public CondExpression (java.util.List<hydra.lisp.syntax.CondClause> clauses, hydra.util.Maybe<hydra.lisp.syntax.Expression> default_) {
    this.clauses = clauses;
    this.default_ = default_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CondExpression)) {
      return false;
    }
    CondExpression o = (CondExpression) other;
    return java.util.Objects.equals(
      this.clauses,
      o.clauses) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(clauses) + 3 * java.util.Objects.hashCode(default_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CondExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      clauses,
      other.clauses);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      default_,
      other.default_);
  }

  public CondExpression withClauses(java.util.List<hydra.lisp.syntax.CondClause> clauses) {
    return new CondExpression(clauses, default_);
  }

  public CondExpression withDefault(hydra.util.Maybe<hydra.lisp.syntax.Expression> default_) {
    return new CondExpression(clauses, default_);
  }
}
