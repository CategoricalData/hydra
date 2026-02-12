// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ExplicitConstructorInvocation implements Serializable, Comparable<ExplicitConstructorInvocation> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ExplicitConstructorInvocation");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final java.util.List<hydra.ext.java.syntax.Expression> arguments;
  
  public final hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant variant;
  
  public ExplicitConstructorInvocation (java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, java.util.List<hydra.ext.java.syntax.Expression> arguments, hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant variant) {
    this.typeArguments = typeArguments;
    this.arguments = arguments;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExplicitConstructorInvocation)) {
      return false;
    }
    ExplicitConstructorInvocation o = (ExplicitConstructorInvocation) other;
    return java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments) && java.util.Objects.equals(
      this.arguments,
      o.arguments) && java.util.Objects.equals(
      this.variant,
      o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeArguments) + 3 * java.util.Objects.hashCode(arguments) + 5 * java.util.Objects.hashCode(variant);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExplicitConstructorInvocation other) {
    int cmp = 0;
    cmp = Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      arguments.hashCode(),
      other.arguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variant).compareTo(other.variant);
  }
  
  public ExplicitConstructorInvocation withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }
  
  public ExplicitConstructorInvocation withArguments(java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }
  
  public ExplicitConstructorInvocation withVariant(hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant variant) {
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }
}
