package hydra.ext.java.syntax;

public class InstanceInitializer {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.InstanceInitializer");
  
  public final hydra.ext.java.syntax.Block value;
  
  public InstanceInitializer (hydra.ext.java.syntax.Block value) {
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