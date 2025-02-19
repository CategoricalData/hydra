// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_Super implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_Super");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_SUPER = new hydra.core.Name("super");
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final Boolean super_;
  
  public MethodReference_Super (java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier, Boolean super_) {
    java.util.Objects.requireNonNull((typeArguments));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((super_));
    this.typeArguments = typeArguments;
    this.identifier = identifier;
    this.super_ = super_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Super)) {
      return false;
    }
    MethodReference_Super o = (MethodReference_Super) (other);
    return typeArguments.equals(o.typeArguments) && identifier.equals(o.identifier) && super_.equals(o.super_);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeArguments.hashCode() + 3 * identifier.hashCode() + 5 * super_.hashCode();
  }
  
  public MethodReference_Super withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
  
  public MethodReference_Super withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
  
  public MethodReference_Super withSuper(Boolean super_) {
    java.util.Objects.requireNonNull((super_));
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
}