// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class Unwind implements Serializable, Comparable<Unwind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.Unwind");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public final hydra.cypher.openCypher.Expression expression;

  public final hydra.cypher.openCypher.Variable variable;

  public Unwind (hydra.cypher.openCypher.Expression expression, hydra.cypher.openCypher.Variable variable) {
    this.expression = expression;
    this.variable = variable;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Unwind)) {
      return false;
    }
    Unwind o = (Unwind) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.variable,
      o.variable);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(variable);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Unwind other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variable,
      other.variable);
  }

  public Unwind withExpression(hydra.cypher.openCypher.Expression expression) {
    return new Unwind(expression, variable);
  }

  public Unwind withVariable(hydra.cypher.openCypher.Variable variable) {
    return new Unwind(expression, variable);
  }
}
