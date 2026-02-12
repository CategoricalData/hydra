// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Listcomp implements Serializable, Comparable<Listcomp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Listcomp");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_FOR_IF_CLAUSES = new hydra.core.Name("forIfClauses");
  
  public final hydra.ext.python.syntax.NamedExpression expression;
  
  public final hydra.ext.python.syntax.ForIfClauses forIfClauses;
  
  public Listcomp (hydra.ext.python.syntax.NamedExpression expression, hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    this.expression = expression;
    this.forIfClauses = forIfClauses;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Listcomp)) {
      return false;
    }
    Listcomp o = (Listcomp) other;
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
  public int compareTo(Listcomp other) {
    int cmp = 0;
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) forIfClauses).compareTo(other.forIfClauses);
  }
  
  public Listcomp withExpression(hydra.ext.python.syntax.NamedExpression expression) {
    return new Listcomp(expression, forIfClauses);
  }
  
  public Listcomp withForIfClauses(hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    return new Listcomp(expression, forIfClauses);
  }
}
