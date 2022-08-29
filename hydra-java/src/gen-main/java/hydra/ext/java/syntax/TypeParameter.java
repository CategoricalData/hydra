package hydra.ext.java.syntax;

public class TypeParameter {
  public final java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.Optional<hydra.ext.java.syntax.TypeBound> bound;
  
  public TypeParameter (java.util.List<hydra.ext.java.syntax.TypeParameterModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.Optional<hydra.ext.java.syntax.TypeBound> bound) {
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
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withBound(java.util.Optional<hydra.ext.java.syntax.TypeBound> bound) {
    return new TypeParameter(modifiers, identifier, bound);
  }
}