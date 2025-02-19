// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RefLocalVariableDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefLocalVariableDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.Identifier left;
  
  public final hydra.ext.csharp.syntax.VariableReference right;
  
  public RefLocalVariableDeclarator (hydra.ext.csharp.syntax.Identifier left, hydra.ext.csharp.syntax.VariableReference right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RefLocalVariableDeclarator)) {
      return false;
    }
    RefLocalVariableDeclarator o = (RefLocalVariableDeclarator) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public RefLocalVariableDeclarator withLeft(hydra.ext.csharp.syntax.Identifier left) {
    java.util.Objects.requireNonNull((left));
    return new RefLocalVariableDeclarator(left, right);
  }
  
  public RefLocalVariableDeclarator withRight(hydra.ext.csharp.syntax.VariableReference right) {
    java.util.Objects.requireNonNull((right));
    return new RefLocalVariableDeclarator(left, right);
  }
}