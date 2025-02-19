// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class BinaryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.query.BinaryExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.pg.query.Expression left;
  
  public final hydra.pg.query.BinaryOperator operator;
  
  public final hydra.pg.query.Expression right;
  
  public BinaryExpression (hydra.pg.query.Expression left, hydra.pg.query.BinaryOperator operator, hydra.pg.query.Expression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryExpression)) {
      return false;
    }
    BinaryExpression o = (BinaryExpression) (other);
    return left.equals(o.left) && operator.equals(o.operator) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * operator.hashCode() + 5 * right.hashCode();
  }
  
  public BinaryExpression withLeft(hydra.pg.query.Expression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryExpression(left, operator, right);
  }
  
  public BinaryExpression withOperator(hydra.pg.query.BinaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryExpression(left, operator, right);
  }
  
  public BinaryExpression withRight(hydra.pg.query.Expression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryExpression(left, operator, right);
  }
}