// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TypeParameter implements Serializable, Comparable<TypeParameter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.TypeParameter");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.TypeBound> bound;
  
  public TypeParameter (java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, hydra.util.Maybe<hydra.ext.java.syntax.TypeBound> bound) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.bound = bound;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeParameter)) {
      return false;
    }
    TypeParameter o = (TypeParameter) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.bound,
      o.bound);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(identifier) + 5 * java.util.Objects.hashCode(bound);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeParameter other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      bound.hashCode(),
      other.bound.hashCode());
  }
  
  public TypeParameter withModifiers(java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers) {
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withBound(hydra.util.Maybe<hydra.ext.java.syntax.TypeBound> bound) {
    return new TypeParameter(modifiers, identifier, bound);
  }
}
