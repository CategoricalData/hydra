package hydra.ext.tinkerpop.typed;

/**
 * A literal value representing an edge id
 */
public class EdgeId {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/tinkerpop/typed.EdgeId");
  
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