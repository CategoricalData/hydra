// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PartialComparisonExpression implements Serializable, Comparable<PartialComparisonExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PartialComparisonExpression");
  
  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.cypher.openCypher.ComparisonOperator operator;
  
  public final hydra.ext.cypher.openCypher.StringListNullPredicateExpression right;
  
  public PartialComparisonExpression (hydra.ext.cypher.openCypher.ComparisonOperator operator, hydra.ext.cypher.openCypher.StringListNullPredicateExpression right) {
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PartialComparisonExpression)) {
      return false;
    }
    PartialComparisonExpression o = (PartialComparisonExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.right,
      o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(right);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PartialComparisonExpression other) {
    int cmp = 0;
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) right).compareTo(other.right);
  }
  
  public PartialComparisonExpression withOperator(hydra.ext.cypher.openCypher.ComparisonOperator operator) {
    return new PartialComparisonExpression(operator, right);
  }
  
  public PartialComparisonExpression withRight(hydra.ext.cypher.openCypher.StringListNullPredicateExpression right) {
    return new PartialComparisonExpression(operator, right);
  }
}
