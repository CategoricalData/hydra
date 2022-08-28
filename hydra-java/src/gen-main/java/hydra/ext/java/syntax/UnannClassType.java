package hydra.ext.java.syntax;

/**
 * A ClassType which does not allow annotations
 */
public class UnannClassType {
  /**
   * A ClassType which does not allow annotations
   */
  public final hydra.ext.java.syntax.ClassType value;
  
  public UnannClassType (hydra.ext.java.syntax.ClassType value) {
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