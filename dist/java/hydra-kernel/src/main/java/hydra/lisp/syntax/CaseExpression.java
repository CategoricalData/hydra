// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * Case dispatch on a value. Serializes as (case x key1 expr1 key2 expr2 default) in Clojure, (case x (key1 expr1) (key2 expr2) (otherwise default)) in Common Lisp, (case x ((key1) expr1) ((key2) expr2) (else default)) in Scheme
 */
public class CaseExpression implements Serializable, Comparable<CaseExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.CaseExpression");

  public static final hydra.core.Name SCRUTINEE = new hydra.core.Name("scrutinee");

  public static final hydra.core.Name CLAUSES = new hydra.core.Name("clauses");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  /**
   * The expression being dispatched on
   */
  public final hydra.lisp.syntax.Expression scrutinee;

  /**
   * The case clauses
   */
  public final java.util.List<hydra.lisp.syntax.CaseClause> clauses;

  /**
   * Optional default clause
   */
  public final hydra.util.Maybe<hydra.lisp.syntax.Expression> default_;

  public CaseExpression (hydra.lisp.syntax.Expression scrutinee, java.util.List<hydra.lisp.syntax.CaseClause> clauses, hydra.util.Maybe<hydra.lisp.syntax.Expression> default_) {
    this.scrutinee = scrutinee;
    this.clauses = clauses;
    this.default_ = default_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseExpression)) {
      return false;
    }
    CaseExpression o = (CaseExpression) other;
    return java.util.Objects.equals(
      this.scrutinee,
      o.scrutinee) && java.util.Objects.equals(
      this.clauses,
      o.clauses) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scrutinee) + 3 * java.util.Objects.hashCode(clauses) + 5 * java.util.Objects.hashCode(default_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      scrutinee,
      other.scrutinee);
    if (cmp != 0) {
      return cmp;
    }
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

  public CaseExpression withScrutinee(hydra.lisp.syntax.Expression scrutinee) {
    return new CaseExpression(scrutinee, clauses, default_);
  }

  public CaseExpression withClauses(java.util.List<hydra.lisp.syntax.CaseClause> clauses) {
    return new CaseExpression(scrutinee, clauses, default_);
  }

  public CaseExpression withDefault(hydra.util.Maybe<hydra.lisp.syntax.Expression> default_) {
    return new CaseExpression(scrutinee, clauses, default_);
  }
}
