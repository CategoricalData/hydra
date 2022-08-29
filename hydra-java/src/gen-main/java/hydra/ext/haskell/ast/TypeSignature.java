package hydra.ext.haskell.ast;

public class TypeSignature {
  public final hydra.ext.haskell.ast.Name name;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public TypeSignature (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSignature)) {
      return false;
    }
    TypeSignature o = (TypeSignature) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public TypeSignature withName(hydra.ext.haskell.ast.Name name) {
    return new TypeSignature(name, type);
  }
  
  public TypeSignature withType(hydra.ext.haskell.ast.Type type) {
    return new TypeSignature(name, type);
  }
}