// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayAccess implements Serializable, Comparable<ArrayAccess> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayAccess");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> expression;
  
  public final hydra.ext.java.syntax.ArrayAccess_Variant variant;
  
  public ArrayAccess (hydra.util.Maybe<hydra.ext.java.syntax.Expression> expression, hydra.ext.java.syntax.ArrayAccess_Variant variant) {
    this.expression = expression;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayAccess)) {
      return false;
    }
    ArrayAccess o = (ArrayAccess) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.variant,
      o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(variant);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ArrayAccess other) {
    int cmp = 0;
    cmp = Integer.compare(
      expression.hashCode(),
      other.expression.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variant).compareTo(other.variant);
  }
  
  public ArrayAccess withExpression(hydra.util.Maybe<hydra.ext.java.syntax.Expression> expression) {
    return new ArrayAccess(expression, variant);
  }
  
  public ArrayAccess withVariant(hydra.ext.java.syntax.ArrayAccess_Variant variant) {
    return new ArrayAccess(expression, variant);
  }
}
