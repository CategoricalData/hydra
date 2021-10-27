package hydra.ext.tinkerpop.v3;

/**
 * The type of a reference to a vertex by id
 */
public class VertexIdType {
  public final hydra.ext.tinkerpop.v3.VertexType vertexType;
  
  /**
   * Constructs an immutable VertexIdType object
   */
  public VertexIdType(hydra.ext.tinkerpop.v3.VertexType vertexType) {
    this.vertexType = vertexType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexIdType)) {
        return false;
    }
    VertexIdType o = (VertexIdType) other;
    return vertexType.equals(o.vertexType);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertexType.hashCode();
  }
}
