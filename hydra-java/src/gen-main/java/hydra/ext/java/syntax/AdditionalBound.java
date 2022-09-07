package hydra.ext.java.syntax;

public class AdditionalBound {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.AdditionalBound");
  
  public final hydra.ext.java.syntax.InterfaceType value;
  
  public AdditionalBound (hydra.ext.java.syntax.InterfaceType value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdditionalBound)) {
      return false;
    }
    AdditionalBound o = (AdditionalBound) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}