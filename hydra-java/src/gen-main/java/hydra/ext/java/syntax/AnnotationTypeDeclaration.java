package hydra.ext.java.syntax;

public class AnnotationTypeDeclaration {
  public final java.util.List<InterfaceModifier> modifiers;
  
  public final TypeIdentifier identifier;
  
  public final AnnotationTypeBody body;
  
  public AnnotationTypeDeclaration (java.util.List<InterfaceModifier> modifiers, TypeIdentifier identifier, AnnotationTypeBody body) {
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
  
  public AnnotationTypeDeclaration withModifiers(java.util.List<InterfaceModifier> modifiers) {
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
  
  public AnnotationTypeDeclaration withIdentifier(TypeIdentifier identifier) {
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
  
  public AnnotationTypeDeclaration withBody(AnnotationTypeBody body) {
    return new AnnotationTypeDeclaration(modifiers, identifier, body);
  }
}