package hydra.ext.java.syntax;

public class ArrayInitializer {
  public final java.util.List<java.util.List<VariableInitializer>> value;
  
  public ArrayInitializer (java.util.List<java.util.List<VariableInitializer>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayInitializer)) {
      return false;
    }
    ArrayInitializer o = (ArrayInitializer) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}