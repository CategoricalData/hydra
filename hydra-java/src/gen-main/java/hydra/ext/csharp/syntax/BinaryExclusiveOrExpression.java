// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryExclusiveOrExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryExclusiveOrExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.ExclusiveOrExpression left;
  
  public final hydra.ext.csharp.syntax.AndExpression right;
  
  public BinaryExclusiveOrExpression (hydra.ext.csharp.syntax.ExclusiveOrExpression left, hydra.ext.csharp.syntax.AndExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryExclusiveOrExpression)) {
      return false;
    }
    BinaryExclusiveOrExpression o = (BinaryExclusiveOrExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public BinaryExclusiveOrExpression withLeft(hydra.ext.csharp.syntax.ExclusiveOrExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryExclusiveOrExpression(left, right);
  }
  
  public BinaryExclusiveOrExpression withRight(hydra.ext.csharp.syntax.AndExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryExclusiveOrExpression(left, right);
  }
}