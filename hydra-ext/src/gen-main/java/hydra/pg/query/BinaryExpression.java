// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class BinaryExpression implements Serializable, Comparable<BinaryExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.BinaryExpression");
  
  public static final hydra.core.Name LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");
  
  public final hydra.pg.query.Expression left;
  
  public final hydra.pg.query.BinaryOperator operator;
  
  public final hydra.pg.query.Expression right;
  
  public BinaryExpression (hydra.pg.query.Expression left, hydra.pg.query.BinaryOperator operator, hydra.pg.query.Expression right) {
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryExpression)) {
      return false;
    }
    BinaryExpression o = (BinaryExpression) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.right,
      o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(operator) + 5 * java.util.Objects.hashCode(right);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BinaryExpression other) {
    int cmp = 0;
    cmp = ((Comparable) left).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) right).compareTo(other.right);
  }
  
  public BinaryExpression withLeft(hydra.pg.query.Expression left) {
    return new BinaryExpression(left, operator, right);
  }
  
  public BinaryExpression withOperator(hydra.pg.query.BinaryOperator operator) {
    return new BinaryExpression(left, operator, right);
  }
  
  public BinaryExpression withRight(hydra.pg.query.Expression right) {
    return new BinaryExpression(left, operator, right);
  }
}
