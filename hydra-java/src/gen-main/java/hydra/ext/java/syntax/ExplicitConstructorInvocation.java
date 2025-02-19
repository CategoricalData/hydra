// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ExplicitConstructorInvocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ExplicitConstructorInvocation");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final java.util.List<hydra.ext.java.syntax.Expression> arguments;
  
  public final hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant variant;
  
  public ExplicitConstructorInvocation (java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, java.util.List<hydra.ext.java.syntax.Expression> arguments, hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant variant) {
    java.util.Objects.requireNonNull((typeArguments));
    java.util.Objects.requireNonNull((arguments));
    java.util.Objects.requireNonNull((variant));
    this.typeArguments = typeArguments;
    this.arguments = arguments;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExplicitConstructorInvocation)) {
      return false;
    }
    ExplicitConstructorInvocation o = (ExplicitConstructorInvocation) (other);
    return typeArguments.equals(o.typeArguments) && arguments.equals(o.arguments) && variant.equals(o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeArguments.hashCode() + 3 * arguments.hashCode() + 5 * variant.hashCode();
  }
  
  public ExplicitConstructorInvocation withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }
  
  public ExplicitConstructorInvocation withArguments(java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }
  
  public ExplicitConstructorInvocation withVariant(hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant variant) {
    java.util.Objects.requireNonNull((variant));
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }
}