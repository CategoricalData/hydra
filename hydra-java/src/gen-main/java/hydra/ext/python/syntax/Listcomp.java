// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Listcomp implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Listcomp");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_FOR_IF_CLAUSES = new hydra.core.Name("forIfClauses");
  
  public final hydra.ext.python.syntax.NamedExpression expression;
  
  public final hydra.ext.python.syntax.ForIfClauses forIfClauses;
  
  public Listcomp (hydra.ext.python.syntax.NamedExpression expression, hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((forIfClauses));
    this.expression = expression;
    this.forIfClauses = forIfClauses;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Listcomp)) {
      return false;
    }
    Listcomp o = (Listcomp) (other);
    return expression.equals(o.expression) && forIfClauses.equals(o.forIfClauses);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * forIfClauses.hashCode();
  }
  
  public Listcomp withExpression(hydra.ext.python.syntax.NamedExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new Listcomp(expression, forIfClauses);
  }
  
  public Listcomp withForIfClauses(hydra.ext.python.syntax.ForIfClauses forIfClauses) {
    java.util.Objects.requireNonNull((forIfClauses));
    return new Listcomp(expression, forIfClauses);
  }
}