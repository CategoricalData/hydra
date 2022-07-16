package hydra.ext.java.syntax;

public class NormalInterfaceDeclaration {
  public final java.util.List<InterfaceModifier> modifiers;
  
  public final TypeIdentifier identifier;
  
  public final java.util.List<TypeParameter> parameters;
  
  public final java.util.List<InterfaceType> extends_;
  
  public final InterfaceBody body;
  
  public NormalInterfaceDeclaration (java.util.List<InterfaceModifier> modifiers, TypeIdentifier identifier, java.util.List<TypeParameter> parameters, java.util.List<InterfaceType> extends_, InterfaceBody body) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.parameters = parameters;
    this.extends_ = extends_;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalInterfaceDeclaration)) {
      return false;
    }
    NormalInterfaceDeclaration o = (NormalInterfaceDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && parameters.equals(o.parameters) && extends_.equals(o.extends_) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * parameters.hashCode() + 7 * extends_.hashCode() + 11 * body.hashCode();
  }
  
  public NormalInterfaceDeclaration withModifiers(java.util.List<InterfaceModifier> modifiers) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withIdentifier(TypeIdentifier identifier) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withParameters(java.util.List<TypeParameter> parameters) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withExtends(java.util.List<InterfaceType> extends_) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
  
  public NormalInterfaceDeclaration withBody(InterfaceBody body) {
    return new NormalInterfaceDeclaration(modifiers, identifier, parameters, extends_, body);
  }
}