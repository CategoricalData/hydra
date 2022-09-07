package hydra.ext.coq.syntax;

public class TypeBinders {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.TypeBinders");
  
  public final java.util.List<hydra.ext.coq.syntax.Name> names;
  
  public final hydra.ext.coq.syntax.Type type;
  
  public TypeBinders (java.util.List<hydra.ext.coq.syntax.Name> names, hydra.ext.coq.syntax.Type type) {
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
  
  public TypeBinders withNames(java.util.List<hydra.ext.coq.syntax.Name> names) {
    return new TypeBinders(names, type);
  }
  
  public TypeBinders withType(hydra.ext.coq.syntax.Type type) {
    return new TypeBinders(names, type);
  }
}