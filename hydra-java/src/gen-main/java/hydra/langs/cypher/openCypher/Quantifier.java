package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Quantifier implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Quantifier");
  
  public final hydra.langs.cypher.openCypher.QuantifierOperator operator;
  
  public final hydra.langs.cypher.openCypher.FilterExpression expression;
  
  public Quantifier (hydra.langs.cypher.openCypher.QuantifierOperator operator, hydra.langs.cypher.openCypher.FilterExpression expression) {
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
    return new Quantifier(operator, expression);
  }
  
  public Quantifier withExpression(hydra.langs.cypher.openCypher.FilterExpression expression) {
    return new Quantifier(operator, expression);
  }
}