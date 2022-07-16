package hydra.ext.java.syntax;

/**
 * A Type which does not allow annotations
 */
public class UnannType {
  /**
   * A Type which does not allow annotations
   */
  public final Type value;
  
  public UnannType (Type value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnannType)) {
      return false;
    }
    UnannType o = (UnannType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}