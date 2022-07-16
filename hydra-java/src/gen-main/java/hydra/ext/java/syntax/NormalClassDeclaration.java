package hydra.ext.java.syntax;

public class NormalClassDeclaration {
  public final java.util.List<ClassModifier> modifiers;
  
  public final TypeIdentifier identifier;
  
  public final java.util.List<TypeParameter> parameters;
  
  public final java.util.Optional<ClassType> extends_;
  
  public final java.util.List<InterfaceType> implements_;
  
  public final ClassBody body;
  
  public NormalClassDeclaration (java.util.List<ClassModifier> modifiers, TypeIdentifier identifier, java.util.List<TypeParameter> parameters, java.util.Optional<ClassType> extends_, java.util.List<InterfaceType> implements_, ClassBody body) {
    this.modifiers = modifiers;
    this.identifier = identifier;
    this.parameters = parameters;
    this.extends_ = extends_;
    this.implements_ = implements_;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalClassDeclaration)) {
      return false;
    }
    NormalClassDeclaration o = (NormalClassDeclaration) (other);
    return modifiers.equals(o.modifiers) && identifier.equals(o.identifier) && parameters.equals(o.parameters) && extends_.equals(o.extends_) && implements_.equals(o.implements_) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * identifier.hashCode() + 5 * parameters.hashCode() + 7 * extends_.hashCode() + 11 * implements_.hashCode() + 13 * body.hashCode();
  }
  
  public NormalClassDeclaration withModifiers(java.util.List<ClassModifier> modifiers) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withIdentifier(TypeIdentifier identifier) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withParameters(java.util.List<TypeParameter> parameters) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withExtends(java.util.Optional<ClassType> extends_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withImplements(java.util.List<InterfaceType> implements_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withBody(ClassBody body) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
}