package hydra.ext.coq.syntax;

public class Type {
  public final Term value;
  
  public Type (Term value) {
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