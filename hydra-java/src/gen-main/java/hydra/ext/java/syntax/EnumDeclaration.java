package hydra.ext.java.syntax;

public class EnumDeclaration {
  public final java.util.List<ClassModifier> modifiers;
  
  public final TypeIdentifier identifier;
  
  public final java.util.List<InterfaceType> implements_;
  
  public final EnumBody body;
  
  public EnumDeclaration (java.util.List<ClassModifier> modifiers, TypeIdentifier identifier, java.util.List<InterfaceType> implements_, EnumBody body) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.implements_ = implements_;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumDeclaration)) {
      return false;
    }
    EnumDeclaration o = (EnumDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && implements_.equals(o.implements_) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * implements_.hashCode() + 7 * body.hashCode();
  }
  
  public EnumDeclaration withModifiers(java.util.List<ClassModifier> modifiers) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withIdentifier(TypeIdentifier identifier) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withImplements(java.util.List<InterfaceType> implements_) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withBody(EnumBody body) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
}