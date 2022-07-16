package hydra.ext.java.syntax;

public class MethodName {
  public final Identifier value;
  
  public MethodName (Identifier value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodName)) {
      return false;
    }
    MethodName o = (MethodName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}