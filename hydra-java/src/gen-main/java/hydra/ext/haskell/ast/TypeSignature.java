package hydra.ext.haskell.ast;

public class TypeSignature {
  public final Name name;
  
  public final Type type;
  
  public TypeSignature (Name name, Type type) {
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
  
  public TypeSignature withName(Name name) {
    return new TypeSignature(name, type);
  }
  
  public TypeSignature withType(Type type) {
    return new TypeSignature(name, type);
  }
}