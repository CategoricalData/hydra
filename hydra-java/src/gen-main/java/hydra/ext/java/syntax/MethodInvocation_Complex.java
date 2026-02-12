// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodInvocation_Complex implements Serializable, Comparable<MethodInvocation_Complex> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Complex");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.MethodInvocation_Variant variant;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public MethodInvocation_Complex (hydra.ext.java.syntax.MethodInvocation_Variant variant, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier) {
    this.variant = variant;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodInvocation_Complex)) {
      return false;
    }
    MethodInvocation_Complex o = (MethodInvocation_Complex) other;
    return java.util.Objects.equals(
      this.variant,
      o.variant) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variant) + 3 * java.util.Objects.hashCode(typeArguments) + 5 * java.util.Objects.hashCode(identifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodInvocation_Complex other) {
    int cmp = 0;
    cmp = ((Comparable) variant).compareTo(other.variant);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) identifier).compareTo(other.identifier);
  }
  
  public MethodInvocation_Complex withVariant(hydra.ext.java.syntax.MethodInvocation_Variant variant) {
    return new MethodInvocation_Complex(variant, typeArguments, identifier);
  }
  
  public MethodInvocation_Complex withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new MethodInvocation_Complex(variant, typeArguments, identifier);
  }
  
  public MethodInvocation_Complex withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodInvocation_Complex(variant, typeArguments, identifier);
  }
}
