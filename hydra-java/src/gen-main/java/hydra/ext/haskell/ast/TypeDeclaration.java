package hydra.ext.haskell.ast;

public class TypeDeclaration {
  public final hydra.ext.haskell.ast.DeclarationHead name;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public TypeDeclaration (hydra.ext.haskell.ast.DeclarationHead name, hydra.ext.haskell.ast.Type type) {
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
  
  public TypeDeclaration withName(hydra.ext.haskell.ast.DeclarationHead name) {
    return new TypeDeclaration(name, type);
  }
  
  public TypeDeclaration withType(hydra.ext.haskell.ast.Type type) {
    return new TypeDeclaration(name, type);
  }
}