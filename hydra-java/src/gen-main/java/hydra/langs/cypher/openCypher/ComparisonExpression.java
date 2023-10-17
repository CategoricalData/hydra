package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ComparisonExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ComparisonExpression");
  
  public final hydra.langs.cypher.openCypher.StringListNullPredicateExpression primary;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PartialComparisonExpression> comparisons;
  
  public ComparisonExpression (hydra.langs.cypher.openCypher.StringListNullPredicateExpression primary, java.util.List<hydra.langs.cypher.openCypher.PartialComparisonExpression> comparisons) {
    this.primary = primary;
    this.comparisons = comparisons;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComparisonExpression)) {
      return false;
    }
    ComparisonExpression o = (ComparisonExpression) (other);
    return primary.equals(o.primary) && comparisons.equals(o.comparisons);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * comparisons.hashCode();
  }
  
  public ComparisonExpression withPrimary(hydra.langs.cypher.openCypher.StringListNullPredicateExpression primary) {
    return new ComparisonExpression(primary, comparisons);
  }
  
  public ComparisonExpression withComparisons(java.util.List<hydra.langs.cypher.openCypher.PartialComparisonExpression> comparisons) {
    return new ComparisonExpression(primary, comparisons);
  }
}