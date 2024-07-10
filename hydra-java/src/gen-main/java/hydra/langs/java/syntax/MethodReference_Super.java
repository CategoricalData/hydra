// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class MethodReference_Super implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.MethodReference.Super");
  
  public final java.util.List<hydra.langs.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.langs.java.syntax.Identifier identifier;
  
  public final Boolean super_;
  
  public MethodReference_Super (java.util.List<hydra.langs.java.syntax.TypeArgument> typeArguments, hydra.langs.java.syntax.Identifier identifier, Boolean super_) {
    if (typeArguments == null) {
      throw new IllegalArgumentException("null value for 'typeArguments' argument");
    }
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    if (super_ == null) {
      throw new IllegalArgumentException("null value for 'super' argument");
    }
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
  
  public MethodReference_Super withTypeArguments(java.util.List<hydra.langs.java.syntax.TypeArgument> typeArguments) {
    if (typeArguments == null) {
      throw new IllegalArgumentException("null value for 'typeArguments' argument");
    }
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
  
  public MethodReference_Super withIdentifier(hydra.langs.java.syntax.Identifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
  
  public MethodReference_Super withSuper(Boolean super_) {
    if (super_ == null) {
      throw new IllegalArgumentException("null value for 'super' argument");
    }
    return new MethodReference_Super(typeArguments, identifier, super_);
  }
}