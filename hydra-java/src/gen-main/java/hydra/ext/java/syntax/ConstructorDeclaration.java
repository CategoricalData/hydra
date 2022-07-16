package hydra.ext.java.syntax;

public class ConstructorDeclaration {
  public final java.util.List<ConstructorModifier> modifiers;
  
  public final ConstructorDeclarator constructor;
  
  public final java.util.Optional<Throws> throws_;
  
  public final ConstructorBody body;
  
  public ConstructorDeclaration (java.util.List<ConstructorModifier> modifiers, ConstructorDeclarator constructor, java.util.Optional<Throws> throws_, ConstructorBody body) {
    this.modifiers = modifiers;
    this.constructor = constructor;
    this.throws_ = throws_;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorDeclaration)) {
      return false;
    }
    ConstructorDeclaration o = (ConstructorDeclaration) (other);
    return modifiers.equals(o.modifiers) && constructor.equals(o.constructor) && throws_.equals(o.throws_) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * constructor.hashCode() + 5 * throws_.hashCode() + 7 * body.hashCode();
  }
  
  public ConstructorDeclaration withModifiers(java.util.List<ConstructorModifier> modifiers) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withConstructor(ConstructorDeclarator constructor) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withThrows(java.util.Optional<Throws> throws_) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
  
  public ConstructorDeclaration withBody(ConstructorBody body) {
    return new ConstructorDeclaration(modifiers, constructor, throws_, body);
  }
}