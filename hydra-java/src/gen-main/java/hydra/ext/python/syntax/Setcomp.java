// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Setcomp implements Serializable, Comparable<Setcomp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Setcomp");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_FOR_IF_CLAUSES = new hydra.core.Name("forIfClauses");
  
  public final hydra.ext.python.syntax.NamedExpression expression;
  
  public final hydra.ext.python.syntax.ForIfClauses forIfClauses;
  
  public Setcomp (hydra.ext.python.syntax.NamedExpression expression, hydra.ext.python.syntax.ForIfClauses forIfClauses) {
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
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) forIfClauses).compareTo(other.forIfClauses);
  }
  
  public Setcomp withExpression(hydra.ext.python.syntax.NamedExpression expression) {
    return new Setcomp(expression, forIfClauses);
  }
  
  public Setcomp withForIfClauses(hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    return new Setcomp(expression, forIfClauses);
  }
}
