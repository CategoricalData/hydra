// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PartialComparisonExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.PartialComparisonExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.cypher.openCypher.ComparisonOperator operator;
  
  public final hydra.ext.cypher.openCypher.StringListNullPredicateExpression right;
  
  public PartialComparisonExpression (hydra.ext.cypher.openCypher.ComparisonOperator operator, hydra.ext.cypher.openCypher.StringListNullPredicateExpression right) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
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
  
  public PartialComparisonExpression withOperator(hydra.ext.cypher.openCypher.ComparisonOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new PartialComparisonExpression(operator, right);
  }
  
  public PartialComparisonExpression withRight(hydra.ext.cypher.openCypher.StringListNullPredicateExpression right) {
    java.util.Objects.requireNonNull((right));
    return new PartialComparisonExpression(operator, right);
  }
}
