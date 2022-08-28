package hydra.ext.java.syntax;

public class MethodReference_Array {
  public final hydra.ext.java.syntax.ArrayType value;
  
  public MethodReference_Array (hydra.ext.java.syntax.ArrayType value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Array)) {
      return false;
    }
    MethodReference_Array o = (MethodReference_Array) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}