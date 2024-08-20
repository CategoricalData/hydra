// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ComparisonExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.ComparisonExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.cypher.openCypher.StringListNullPredicateExpression left;
  
  public final java.util.List<hydra.ext.cypher.openCypher.PartialComparisonExpression> right;
  
  public ComparisonExpression (hydra.ext.cypher.openCypher.StringListNullPredicateExpression left, java.util.List<hydra.ext.cypher.openCypher.PartialComparisonExpression> right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComparisonExpression)) {
      return false;
    }
    ComparisonExpression o = (ComparisonExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public ComparisonExpression withLeft(hydra.ext.cypher.openCypher.StringListNullPredicateExpression left) {
    java.util.Objects.requireNonNull((left));
    return new ComparisonExpression(left, right);
  }
  
  public ComparisonExpression withRight(java.util.List<hydra.ext.cypher.openCypher.PartialComparisonExpression> right) {
    java.util.Objects.requireNonNull((right));
    return new ComparisonExpression(left, right);
  }
}
