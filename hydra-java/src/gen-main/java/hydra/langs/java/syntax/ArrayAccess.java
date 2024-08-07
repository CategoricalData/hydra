// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ArrayAccess implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ArrayAccess");
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Expression> expression;
  
  public final hydra.langs.java.syntax.ArrayAccess_Variant variant;
  
  public ArrayAccess (hydra.util.Opt<hydra.langs.java.syntax.Expression> expression, hydra.langs.java.syntax.ArrayAccess_Variant variant) {
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
  
  public ArrayAccess withExpression(hydra.util.Opt<hydra.langs.java.syntax.Expression> expression) {
    java.util.Objects.requireNonNull((expression));
    return new ArrayAccess(expression, variant);
  }
  
  public ArrayAccess withVariant(hydra.langs.java.syntax.ArrayAccess_Variant variant) {
    java.util.Objects.requireNonNull((variant));
    return new ArrayAccess(expression, variant);
  }
}