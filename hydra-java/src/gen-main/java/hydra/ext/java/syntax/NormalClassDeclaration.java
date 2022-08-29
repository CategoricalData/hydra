package hydra.ext.java.syntax;

public class NormalClassDeclaration {
  public final java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers;
  
  public final hydra.ext.java.syntax.TypeIdentifier identifier;
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final java.util.Optional<hydra.ext.java.syntax.ClassType> extends_;
  
  public final java.util.List<hydra.ext.java.syntax.InterfaceType> implements_;
  
  public final hydra.ext.java.syntax.ClassBody body;
  
  public NormalClassDeclaration (java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers, hydra.ext.java.syntax.TypeIdentifier identifier, java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, java.util.Optional<hydra.ext.java.syntax.ClassType> extends_, java.util.List<hydra.ext.java.syntax.InterfaceType> implements_, hydra.ext.java.syntax.ClassBody body) {
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
  
  public NormalClassDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.ClassModifier> modifiers) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withIdentifier(hydra.ext.java.syntax.TypeIdentifier identifier) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withExtends(java.util.Optional<hydra.ext.java.syntax.ClassType> extends_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withImplements(java.util.List<hydra.ext.java.syntax.InterfaceType> implements_) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
  
  public NormalClassDeclaration withBody(hydra.ext.java.syntax.ClassBody body) {
    return new NormalClassDeclaration(modifiers, identifier, parameters, extends_, implements_, body);
  }
}