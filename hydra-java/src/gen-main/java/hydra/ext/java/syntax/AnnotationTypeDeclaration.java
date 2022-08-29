package hydra.ext.java.syntax;

public class AnnotationTypeDeclaration {
  public final java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final hydra.ext.java.syntax.AnnotationTypeBody body;
  
  public AnnotationTypeDeclaration (java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, hydra.ext.java.syntax.AnnotationTypeBody body) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationTypeDeclaration)) {
      return false;
    }
    AnnotationTypeDeclaration o = (AnnotationTypeDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * body.hashCode();
  }
  
  public AnnotationTypeDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.InterfaceModifier> modifiers) {
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
  
  public AnnotationTypeDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
  
  public AnnotationTypeDeclaration withBody(hydra.ext.java.syntax.AnnotationTypeBody body) {
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
}