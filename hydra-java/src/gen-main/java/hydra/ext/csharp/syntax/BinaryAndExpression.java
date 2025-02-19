// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryAndExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryAndExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.AndExpression left;
  
  public final hydra.ext.csharp.syntax.EqualityExpression right;
  
  public BinaryAndExpression (hydra.ext.csharp.syntax.AndExpression left, hydra.ext.csharp.syntax.EqualityExpression right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryAndExpression)) {
      return false;
    }
    BinaryAndExpression o = (BinaryAndExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public BinaryAndExpression withLeft(hydra.ext.csharp.syntax.AndExpression left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryAndExpression(left, right);
  }
  
  public BinaryAndExpression withRight(hydra.ext.csharp.syntax.EqualityExpression right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryAndExpression(left, right);
  }
}