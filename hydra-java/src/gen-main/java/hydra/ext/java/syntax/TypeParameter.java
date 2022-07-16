package hydra.ext.java.syntax;

public class TypeParameter {
  public final java.util.List<TypeParameterModifier> modifiers;
  
  public final TypeIdentifier identifier;
  
  public final java.util.Optional<TypeBound> bound;
  
  public TypeParameter (java.util.List<TypeParameterModifier> modifiers, TypeIdentifier identifier, java.util.Optional<TypeBound> bound) {
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
  
  public TypeParameter withModifiers(java.util.List<TypeParameterModifier> modifiers) {
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withIdentifier(TypeIdentifier identifier) {
    return new TypeParameter(modifiers, identifier, bound);
  }
  
  public TypeParameter withBound(java.util.Optional<TypeBound> bound) {
    return new TypeParameter(modifiers, identifier, bound);
  }
}