package hydra.ext.coq.syntax;

public class Type {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Type");
  
  public final hydra.ext.coq.syntax.Term value;
  
  public Type (hydra.ext.coq.syntax.Term value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type)) {
      return false;
    }
    Type o = (Type) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}