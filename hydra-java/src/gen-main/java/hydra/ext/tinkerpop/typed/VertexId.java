package hydra.ext.tinkerpop.typed;

/**
 * A literal value representing a vertex id
 */
public class VertexId {
  /**
   * A literal value representing a vertex id
   */
  public final hydra.core.Literal value;
  
  public VertexId (hydra.core.Literal value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexId)) {
      return false;
    }
    VertexId o = (VertexId) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}