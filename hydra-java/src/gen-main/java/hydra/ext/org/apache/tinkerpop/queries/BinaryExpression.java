// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.queries;

import java.io.Serializable;

public class BinaryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/queries.BinaryExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.org.apache.tinkerpop.queries.Expression left;
  
  public final hydra.ext.org.apache.tinkerpop.queries.BinaryOperator operator;
  
  public final hydra.ext.org.apache.tinkerpop.queries.Expression right;
  
  public BinaryExpression (hydra.ext.org.apache.tinkerpop.queries.Expression left, hydra.ext.org.apache.tinkerpop.queries.BinaryOperator operator, hydra.ext.org.apache.tinkerpop.queries.Expression right) {
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
  
  public BinaryExpression withLeft(hydra.ext.org.apache.tinkerpop.queries.Expression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryExpression(left, operator, right);
  }
  
  public BinaryExpression withOperator(hydra.ext.org.apache.tinkerpop.queries.BinaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryExpression(left, operator, right);
  }
  
  public BinaryExpression withRight(hydra.ext.org.apache.tinkerpop.queries.Expression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryExpression(left, operator, right);
  }
}