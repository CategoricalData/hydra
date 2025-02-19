// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BinaryOperatorDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BinaryOperatorDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.OverloadableBinaryOperator operator;
  
  public final hydra.ext.csharp.syntax.FixedParameter left;
  
  public final hydra.ext.csharp.syntax.FixedParameter right;
  
  public BinaryOperatorDeclarator (hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.OverloadableBinaryOperator operator, hydra.ext.csharp.syntax.FixedParameter left, hydra.ext.csharp.syntax.FixedParameter right) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.type = type;
    this.operator = operator;
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryOperatorDeclarator)) {
      return false;
    }
    BinaryOperatorDeclarator o = (BinaryOperatorDeclarator) (other);
    return type.equals(o.type) && operator.equals(o.operator) && left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * operator.hashCode() + 5 * left.hashCode() + 7 * right.hashCode();
  }
  
  public BinaryOperatorDeclarator withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new BinaryOperatorDeclarator(type, operator, left, right);
  }
  
  public BinaryOperatorDeclarator withOperator(hydra.ext.csharp.syntax.OverloadableBinaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new BinaryOperatorDeclarator(type, operator, left, right);
  }
  
  public BinaryOperatorDeclarator withLeft(hydra.ext.csharp.syntax.FixedParameter left) {
    java.util.Objects.requireNonNull((left));
    return new BinaryOperatorDeclarator(type, operator, left, right);
  }
  
  public BinaryOperatorDeclarator withRight(hydra.ext.csharp.syntax.FixedParameter right) {
    java.util.Objects.requireNonNull((right));
    return new BinaryOperatorDeclarator(type, operator, left, right);
  }
}