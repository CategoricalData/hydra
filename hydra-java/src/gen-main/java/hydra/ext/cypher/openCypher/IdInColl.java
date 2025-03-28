// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class IdInColl implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.IdInColl");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.cypher.openCypher.Variable variable;
  
  public final hydra.ext.cypher.openCypher.Expression expression;
  
  public IdInColl (hydra.ext.cypher.openCypher.Variable variable, hydra.ext.cypher.openCypher.Expression expression) {
    java.util.Objects.requireNonNull((variable));
    java.util.Objects.requireNonNull((expression));
    this.variable = variable;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IdInColl)) {
      return false;
    }
    IdInColl o = (IdInColl) (other);
    return variable.equals(o.variable) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * expression.hashCode();
  }
  
  public IdInColl withVariable(hydra.ext.cypher.openCypher.Variable variable) {
    java.util.Objects.requireNonNull((variable));
    return new IdInColl(variable, expression);
  }
  
  public IdInColl withExpression(hydra.ext.cypher.openCypher.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new IdInColl(variable, expression);
  }
}