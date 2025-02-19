// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class StringPredicateExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.StringPredicateExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.cypher.openCypher.StringPredicateOperator operator;
  
  public final hydra.ext.cypher.openCypher.AddOrSubtractExpression expression;
  
  public StringPredicateExpression (hydra.ext.cypher.openCypher.StringPredicateOperator operator, hydra.ext.cypher.openCypher.AddOrSubtractExpression expression) {
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
  
  public StringPredicateExpression withOperator(hydra.ext.cypher.openCypher.StringPredicateOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new StringPredicateExpression(operator, expression);
  }
  
  public StringPredicateExpression withExpression(hydra.ext.cypher.openCypher.AddOrSubtractExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new StringPredicateExpression(operator, expression);
  }
}