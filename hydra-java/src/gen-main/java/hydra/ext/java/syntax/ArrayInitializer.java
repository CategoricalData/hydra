package hydra.ext.java.syntax;

public class ArrayInitializer {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ArrayInitializer");
  
  public final java.util.List<java.util.List<hydra.ext.java.syntax.VariableInitializer>> value;
  
  public ArrayInitializer (java.util.List<java.util.List<hydra.ext.java.syntax.VariableInitializer>> value) {
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