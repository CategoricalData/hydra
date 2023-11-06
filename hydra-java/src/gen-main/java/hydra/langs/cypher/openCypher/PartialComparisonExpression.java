package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PartialComparisonExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PartialComparisonExpression");
  
  public final hydra.langs.cypher.openCypher.ComparisonOperator operator;
  
  public final hydra.langs.cypher.openCypher.StringListNullPredicateExpression expression;
  
  public PartialComparisonExpression (hydra.langs.cypher.openCypher.ComparisonOperator operator, hydra.langs.cypher.openCypher.StringListNullPredicateExpression expression) {
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PartialComparisonExpression)) {
      return false;
    }
    PartialComparisonExpression o = (PartialComparisonExpression) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public PartialComparisonExpression withOperator(hydra.langs.cypher.openCypher.ComparisonOperator operator) {
    return new PartialComparisonExpression(operator, expression);
  }
  
  public PartialComparisonExpression withExpression(hydra.langs.cypher.openCypher.StringListNullPredicateExpression expression) {
    return new PartialComparisonExpression(operator, expression);
  }
}