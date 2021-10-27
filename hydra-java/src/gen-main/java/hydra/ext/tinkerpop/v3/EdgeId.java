package hydra.ext.tinkerpop.v3;

public class EdgeId {
  public final hydra.ext.tinkerpop.v3.Value value;
  
  /**
   * Constructs an immutable EdgeId object
   */
  public EdgeId(hydra.ext.tinkerpop.v3.Value value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeId)) {
        return false;
    }
    EdgeId o = (EdgeId) other;
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
