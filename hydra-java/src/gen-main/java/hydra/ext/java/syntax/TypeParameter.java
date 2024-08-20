// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TypeParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.TypeParameter");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.TypeBound> bound;
  
  public TypeParameter (java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, hydra.util.Opt<hydra.ext.java.syntax.TypeBound> bound) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((bound));
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.bound = bound;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeParameter)) {
      return false;
    }
    TypeParameter o = (TypeParameter) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && bound.equals(o.bound);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * bound.hashCode();
  }
  
  public TypeParameter withModifiers(java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withBound(hydra.util.Opt<hydra.ext.java.syntax.TypeBound> bound) {
    java.util.Objects.requireNonNull((bound));
    return new TypeParameter(modifiers, identifier, bound);
  }
}
