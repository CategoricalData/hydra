package hydra.ext.java.syntax;

public class InstanceInitializer {
  public final Block value;
  
  public InstanceInitializer (Block value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InstanceInitializer)) {
      return false;
    }
    InstanceInitializer o = (InstanceInitializer) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}