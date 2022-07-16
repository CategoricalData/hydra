package hydra.ext.tinkerpop.v3;

/**
 * A vertex or edge id
 */
public class Id {
  /**
   * A vertex or edge id
   */
  public final hydra.core.Literal value;
  
  public Id (hydra.core.Literal value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Id)) {
      return false;
    }
    Id o = (Id) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}