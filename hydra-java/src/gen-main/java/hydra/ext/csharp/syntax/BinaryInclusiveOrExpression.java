// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryInclusiveOrExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryInclusiveOrExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.InclusiveOrExpression left;
  
  public final hydra.ext.csharp.syntax.ExclusiveOrExpression right;
  
  public BinaryInclusiveOrExpression (hydra.ext.csharp.syntax.InclusiveOrExpression left, hydra.ext.csharp.syntax.ExclusiveOrExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryInclusiveOrExpression)) {
      return false;
    }
    BinaryInclusiveOrExpression o = (BinaryInclusiveOrExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public BinaryInclusiveOrExpression withLeft(hydra.ext.csharp.syntax.InclusiveOrExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryInclusiveOrExpression(left, right);
  }
  
  public BinaryInclusiveOrExpression withRight(hydra.ext.csharp.syntax.ExclusiveOrExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryInclusiveOrExpression(left, right);
  }
}