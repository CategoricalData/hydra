// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Quantifier implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.Quantifier");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.cypher.openCypher.QuantifierOperator operator;
  
  public final hydra.ext.cypher.openCypher.FilterExpression expression;
  
  public Quantifier (hydra.ext.cypher.openCypher.QuantifierOperator operator, hydra.ext.cypher.openCypher.FilterExpression expression) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((expression));
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Quantifier)) {
      return false;
    }
    Quantifier o = (Quantifier) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public Quantifier withOperator(hydra.ext.cypher.openCypher.QuantifierOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new Quantifier(operator, expression);
  }
  
  public Quantifier withExpression(hydra.ext.cypher.openCypher.FilterExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new Quantifier(operator, expression);
  }
}
