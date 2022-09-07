package hydra.ext.java.syntax;

public class EnumDeclaration {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.EnumDeclaration");
  
  public final java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.ext.java.syntax.InterfaceType> implements_;
  
  public final hydra.ext.java.syntax.EnumBody body;
  
  public EnumDeclaration (java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.List<hydra.ext.java.syntax.InterfaceType> implements_, hydra.ext.java.syntax.EnumBody body) {
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
  
  public EnumDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withImplements(java.util.List<hydra.ext.java.syntax.InterfaceType> implements_) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
  
  public EnumDeclaration withBody(hydra.ext.java.syntax.EnumBody body) {
    return new EnumDeclaration(modifiers, identifier, implements_, body);
  }
}