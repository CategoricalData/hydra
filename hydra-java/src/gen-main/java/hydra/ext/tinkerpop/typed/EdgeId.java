package hydra.ext.tinkerpop.typed;

/**
 * A literal value representing an edge id
 */
public class EdgeId {
  /**
   * A literal value representing an edge id
   */
  public final hydra.core.Literal value;
  
  public EdgeId (hydra.core.Literal value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeId)) {
      return false;
    }
    EdgeId o = (EdgeId) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}