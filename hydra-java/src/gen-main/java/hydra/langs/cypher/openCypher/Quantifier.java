// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Quantifier implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Quantifier");
  
  public final hydra.langs.cypher.openCypher.QuantifierOperator operator;
  
  public final hydra.langs.cypher.openCypher.FilterExpression expression;
  
  public Quantifier (hydra.langs.cypher.openCypher.QuantifierOperator operator, hydra.langs.cypher.openCypher.FilterExpression expression) {
    if (operator == null) {
      throw new IllegalArgumentException("null value for 'operator' argument");
    }
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
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
  
  public Quantifier withOperator(hydra.langs.cypher.openCypher.QuantifierOperator operator) {
    if (operator == null) {
      throw new IllegalArgumentException("null value for 'operator' argument");
    }
    return new Quantifier(operator, expression);
  }
  
  public Quantifier withExpression(hydra.langs.cypher.openCypher.FilterExpression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new Quantifier(operator, expression);
  }
}