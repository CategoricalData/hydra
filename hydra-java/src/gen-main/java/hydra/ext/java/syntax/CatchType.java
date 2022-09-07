package hydra.ext.java.syntax;

public class CatchType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.CatchType");
  
  public final hydra.ext.java.syntax.UnannClassType type;
  
  public final java.util.List<hydra.ext.java.syntax.ClassType> types;
  
  public CatchType (hydra.ext.java.syntax.UnannClassType type, java.util.List<hydra.ext.java.syntax.ClassType> types) {
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
  
  public CatchType withType(hydra.ext.java.syntax.UnannClassType type) {
    return new CatchType(type, types);
  }
  
  public CatchType withTypes(java.util.List<hydra.ext.java.syntax.ClassType> types) {
    return new CatchType(type, types);
  }
}