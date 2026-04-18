// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class TypeParameter implements Serializable, Comparable<TypeParameter> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.TypeParameter");

  public static final hydra.core.Name MODIFIERS = new hydra.core.Name("modifiers");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public static final hydra.core.Name BOUND = new hydra.core.Name("bound");

  public final java.util.List<hydra.java.syntax.TypeParameterModifier> modifiers;

  public final hydra.java.syntax.TypeIdentifier identifier;

  public final hydra.util.Maybe<hydra.java.syntax.TypeBound> bound;

  public TypeParameter (java.util.List<hydra.java.syntax.TypeParameterModifier> modifiers, hydra.java.syntax.TypeIdentifier identifier, hydra.util.Maybe<hydra.java.syntax.TypeBound> bound) {
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
    cmp = hydra.util.Comparing.compare(
      modifiers,
      other.modifiers);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      identifier,
      other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      bound,
      other.bound);
  }

  public TypeParameter withModifiers(java.util.List<hydra.java.syntax.TypeParameterModifier> modifiers) {
    return new TypeParameter(modifiers, identifier, bound);
  }

  public TypeParameter withIdentifier(hydra.java.syntax.TypeIdentifier identifier) {
    return new TypeParameter(modifiers, identifier, bound);
  }

  public TypeParameter withBound(hydra.util.Maybe<hydra.java.syntax.TypeBound> bound) {
    return new TypeParameter(modifiers, identifier, bound);
  }
}
