// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ArrayAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayAccess");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Expression> expression;
  
  public final hydra.ext.java.syntax.ArrayAccess_Variant variant;
  
  public ArrayAccess (hydra.util.Opt<hydra.ext.java.syntax.Expression> expression, hydra.ext.java.syntax.ArrayAccess_Variant variant) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((variant));
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
  
  public ArrayAccess withExpression(hydra.util.Opt<hydra.ext.java.syntax.Expression> expression) {
    java.util.Objects.requireNonNull((expression));
    return new ArrayAccess(expression, variant);
  }
  
  public ArrayAccess withVariant(hydra.ext.java.syntax.ArrayAccess_Variant variant) {
    java.util.Objects.requireNonNull((variant));
    return new ArrayAccess(expression, variant);
  }
}