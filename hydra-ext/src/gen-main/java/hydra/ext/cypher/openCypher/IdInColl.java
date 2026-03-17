// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class IdInColl implements Serializable, Comparable<IdInColl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.IdInColl");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final hydra.ext.cypher.openCypher.Variable variable;

  public final hydra.ext.cypher.openCypher.Expression expression;

  public IdInColl (hydra.ext.cypher.openCypher.Variable variable, hydra.ext.cypher.openCypher.Expression expression) {
    this.variable = variable;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IdInColl)) {
      return false;
    }
    IdInColl o = (IdInColl) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IdInColl other) {
    int cmp = 0;
    cmp = ((Comparable) variable).compareTo(other.variable);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }

  public IdInColl withVariable(hydra.ext.cypher.openCypher.Variable variable) {
    return new IdInColl(variable, expression);
  }

  public IdInColl withExpression(hydra.ext.cypher.openCypher.Expression expression) {
    return new IdInColl(variable, expression);
  }
}
