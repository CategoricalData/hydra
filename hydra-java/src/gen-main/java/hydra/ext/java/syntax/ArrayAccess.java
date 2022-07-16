package hydra.ext.java.syntax;

public class ArrayAccess {
  public final java.util.Optional<Expression> expression;
  
  public final ArrayAccess_Variant variant;
  
  public ArrayAccess (java.util.Optional<Expression> expression, ArrayAccess_Variant variant) {
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
  
  public ArrayAccess withExpression(java.util.Optional<Expression> expression) {
    return new ArrayAccess(expression, variant);
  }
  
  public ArrayAccess withVariant(ArrayAccess_Variant variant) {
    return new ArrayAccess(expression, variant);
  }
}