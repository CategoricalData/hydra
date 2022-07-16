package hydra.ext.haskell.ast;

public class TypeDeclaration {
  public final DeclarationHead name;
  
  public final Type type;
  
  public TypeDeclaration (DeclarationHead name, Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDeclaration)) {
      return false;
    }
    TypeDeclaration o = (TypeDeclaration) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public TypeDeclaration withName(DeclarationHead name) {
    return new TypeDeclaration(name, type);
  }
  
  public TypeDeclaration withType(Type type) {
    return new TypeDeclaration(name, type);
  }
}