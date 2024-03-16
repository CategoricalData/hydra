package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PartialComparisonExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PartialComparisonExpression");
  
  public final hydra.langs.cypher.openCypher.ComparisonOperator operator;
  
  public final hydra.langs.cypher.openCypher.StringListNullPredicateExpression right;
  
  public PartialComparisonExpression (hydra.langs.cypher.openCypher.ComparisonOperator operator, hydra.langs.cypher.openCypher.StringListNullPredicateExpression right) {
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PartialComparisonExpression)) {
      return false;
    }
    PartialComparisonExpression o = (PartialComparisonExpression) (other);
    return operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * right.hashCode();
  }
  
  public PartialComparisonExpression withOperator(hydra.langs.cypher.openCypher.ComparisonOperator operator) {
    return new PartialComparisonExpression(operator, right);
  }
  
  public PartialComparisonExpression withRight(hydra.langs.cypher.openCypher.StringListNullPredicateExpression right) {
    return new PartialComparisonExpression(operator, right);
  }
}