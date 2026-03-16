// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class StringListNullPredicateExpression implements Serializable, Comparable<StringListNullPredicateExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.StringListNullPredicateExpression");
  
  public static final hydra.core.Name LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.cypher.openCypher.AddOrSubtractExpression left;
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide> right;
  
  public StringListNullPredicateExpression (hydra.ext.cypher.openCypher.AddOrSubtractExpression left, hydra.util.ConsList<hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide> right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringListNullPredicateExpression)) {
      return false;
    }
    StringListNullPredicateExpression o = (StringListNullPredicateExpression) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringListNullPredicateExpression other) {
    int cmp = 0;
    cmp = ((Comparable) left).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) right).compareTo(other.right);
  }
  
  public StringListNullPredicateExpression withLeft(hydra.ext.cypher.openCypher.AddOrSubtractExpression left) {
    return new StringListNullPredicateExpression(left, right);
  }
  
  public StringListNullPredicateExpression withRight(hydra.util.ConsList<hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide> right) {
    return new StringListNullPredicateExpression(left, right);
  }
}
