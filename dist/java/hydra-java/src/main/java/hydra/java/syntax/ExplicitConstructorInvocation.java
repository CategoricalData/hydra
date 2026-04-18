// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ExplicitConstructorInvocation implements Serializable, Comparable<ExplicitConstructorInvocation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ExplicitConstructorInvocation");

  public static final hydra.core.Name TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  public static final hydra.core.Name VARIANT = new hydra.core.Name("variant");

  public final java.util.List<hydra.java.syntax.TypeArgument> typeArguments;

  public final java.util.List<hydra.java.syntax.Expression> arguments;

  public final hydra.java.syntax.ExplicitConstructorInvocation_Variant variant;

  public ExplicitConstructorInvocation (java.util.List<hydra.java.syntax.TypeArgument> typeArguments, java.util.List<hydra.java.syntax.Expression> arguments, hydra.java.syntax.ExplicitConstructorInvocation_Variant variant) {
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
    cmp = hydra.util.Comparing.compare(
      typeArguments,
      other.typeArguments);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      arguments,
      other.arguments);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variant,
      other.variant);
  }

  public ExplicitConstructorInvocation withTypeArguments(java.util.List<hydra.java.syntax.TypeArgument> typeArguments) {
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }

  public ExplicitConstructorInvocation withArguments(java.util.List<hydra.java.syntax.Expression> arguments) {
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }

  public ExplicitConstructorInvocation withVariant(hydra.java.syntax.ExplicitConstructorInvocation_Variant variant) {
    return new ExplicitConstructorInvocation(typeArguments, arguments, variant);
  }
}
