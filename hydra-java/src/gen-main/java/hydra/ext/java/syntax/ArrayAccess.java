package hydra.ext.java.syntax;

public class ArrayAccess {
  public final java.util.Optional<hydra.ext.java.syntax.Expression> expression;
  
  public final hydra.ext.java.syntax.ArrayAccess_Variant variant;
  
  public ArrayAccess (java.util.Optional<hydra.ext.java.syntax.Expression> expression, hydra.ext.java.syntax.ArrayAccess_Variant variant) {
    this.expression = expression;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayAccess)) {
      return false;
    }
    ArrayAccess o = (ArrayAccess) (other);
    return expression.equals(o.expression) && variant.equals(o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * variant.hashCode();
  }
  
  public ArrayAccess withExpression(java.util.Optional<hydra.ext.java.syntax.Expression> expression) {
    return new ArrayAccess(expression, variant);
  }
  
  public ArrayAccess withVariant(hydra.ext.java.syntax.ArrayAccess_Variant variant) {
    return new ArrayAccess(expression, variant);
  }
}