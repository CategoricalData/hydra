package hydra.ext.java.syntax;

public class AdditionalBound {
  public final InterfaceType value;
  
  public AdditionalBound (InterfaceType value) {
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