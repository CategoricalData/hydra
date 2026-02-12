// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_Super implements Serializable, Comparable<MethodReference_Super> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_Super");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_SUPER = new hydra.core.Name("super");
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final Boolean super_;
  
  public MethodReference_Super (java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier, Boolean super_) {
    this.typeArguments = typeArguments;
    this.identifier = identifier;
    this.super_ = super_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Super)) {
      return false;
    }
    MethodReference_Super o = (MethodReference_Super) other;
    return java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.super_,
      o.super_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeArguments) + 3 * java.util.Objects.hashCode(identifier) + 5 * java.util.Objects.hashCode(super_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodReference_Super other) {
    int cmp = 0;
    cmp = Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) super_).compareTo(other.super_);
  }
  
  public MethodReference_Super withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
  
  public MethodReference_Super withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
  
  public MethodReference_Super withSuper(Boolean super_) {
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
}
