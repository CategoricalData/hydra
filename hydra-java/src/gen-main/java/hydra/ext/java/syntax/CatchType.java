package hydra.ext.java.syntax;

public class CatchType {
  public final UnannClassType type;
  
  public final java.util.List<ClassType> types;
  
  public CatchType (UnannClassType type, java.util.List<ClassType> types) {
    this.type = type;
    this.types = types;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchType)) {
      return false;
    }
    CatchType o = (CatchType) (other);
    return type.equals(o.type) && types.equals(o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * types.hashCode();
  }
  
  public CatchType withType(UnannClassType type) {
    return new CatchType(type, types);
  }
  
  public CatchType withTypes(java.util.List<ClassType> types) {
    return new CatchType(type, types);
  }
}