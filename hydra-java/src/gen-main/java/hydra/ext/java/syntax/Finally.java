package hydra.ext.java.syntax;

public class Finally {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Finally");
  
  public final hydra.ext.java.syntax.Block value;
  
  public Finally (hydra.ext.java.syntax.Block value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Finally)) {
      return false;
    }
    Finally o = (Finally) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}