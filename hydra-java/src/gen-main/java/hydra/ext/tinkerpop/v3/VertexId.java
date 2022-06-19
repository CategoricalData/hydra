package hydra.ext.tinkerpop.v3;

/**
 * A vertex id
 */
public class VertexId {
  /**
   * A vertex id
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