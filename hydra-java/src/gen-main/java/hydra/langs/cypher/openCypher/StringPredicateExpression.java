// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StringPredicateExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringPredicateExpression");
  
  public final hydra.langs.cypher.openCypher.StringPredicateOperator operator;
  
  public final hydra.langs.cypher.openCypher.AddOrSubtractExpression expression;
  
  public StringPredicateExpression (hydra.langs.cypher.openCypher.StringPredicateOperator operator, hydra.langs.cypher.openCypher.AddOrSubtractExpression expression) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((expression));
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringPredicateExpression)) {
      return false;
    }
    StringPredicateExpression o = (StringPredicateExpression) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public StringPredicateExpression withOperator(hydra.langs.cypher.openCypher.StringPredicateOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new StringPredicateExpression(operator, expression);
  }
  
  public StringPredicateExpression withExpression(hydra.langs.cypher.openCypher.AddOrSubtractExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new StringPredicateExpression(operator, expression);
  }
}