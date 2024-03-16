package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ComparisonExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ComparisonExpression");
  
  public final hydra.langs.cypher.openCypher.StringListNullPredicateExpression left;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PartialComparisonExpression> right;
  
  public ComparisonExpression (hydra.langs.cypher.openCypher.StringListNullPredicateExpression left, java.util.List<hydra.langs.cypher.openCypher.PartialComparisonExpression> right) {
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
  
  public ComparisonExpression withLeft(hydra.langs.cypher.openCypher.StringListNullPredicateExpression left) {
    return new ComparisonExpression(left, right);
  }
  
  public ComparisonExpression withRight(java.util.List<hydra.langs.cypher.openCypher.PartialComparisonExpression> right) {
    return new ComparisonExpression(left, right);
  }
}