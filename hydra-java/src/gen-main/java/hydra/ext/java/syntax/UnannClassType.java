package hydra.ext.java.syntax;

/**
 * A ClassType which does not allow annotations
 */
public class UnannClassType {
  /**
   * A ClassType which does not allow annotations
   */
  public final ClassType value;
  
  public UnannClassType (ClassType value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnannClassType)) {
      return false;
    }
    UnannClassType o = (UnannClassType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}