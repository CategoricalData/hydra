package hydra.ext.coq.syntax;

public class TypeBinders {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<Name> names;
  
  public final Type type;
  
  public TypeBinders (java.util.List<Name> names, Type type) {
    this.names = names;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeBinders)) {
      return false;
    }
    TypeBinders o = (TypeBinders) (other);
    return names.equals(o.names) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * names.hashCode() + 3 * type.hashCode();
  }
  
  public TypeBinders withNames(java.util.List<Name> names) {
    return new TypeBinders(names, type);
  }
  
  public TypeBinders withType(Type type) {
    return new TypeBinders(names, type);
  }
}