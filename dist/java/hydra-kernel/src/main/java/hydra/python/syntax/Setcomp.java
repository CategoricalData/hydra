// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Setcomp implements Serializable, Comparable<Setcomp> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Setcomp");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name FOR_IF_CLAUSES = new hydra.core.Name("forIfClauses");

  public final hydra.python.syntax.NamedExpression expression;

  public final hydra.python.syntax.ForIfClauses forIfClauses;

  public Setcomp (hydra.python.syntax.NamedExpression expression, hydra.python.syntax.ForIfClauses forIfClauses) {
    this.expression = expression;
    this.forIfClauses = forIfClauses;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Setcomp)) {
      return false;
    }
    Setcomp o = (Setcomp) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.forIfClauses,
      o.forIfClauses);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(forIfClauses);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Setcomp other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      forIfClauses,
      other.forIfClauses);
  }

  public Setcomp withExpression(hydra.python.syntax.NamedExpression expression) {
    return new Setcomp(expression, forIfClauses);
  }

  public Setcomp withForIfClauses(hydra.python.syntax.ForIfClauses forIfClauses) {
    return new Setcomp(expression, forIfClauses);
  }
}
