package hydra.ext.tinkerpop.v3;

/**
 * The type of a reference to an edge by id
 */
public class EdgeIdType {
  public final hydra.ext.tinkerpop.v3.EdgeType edgeType;
  
  /**
   * Constructs an immutable EdgeIdType object
   */
  public EdgeIdType(hydra.ext.tinkerpop.v3.EdgeType edgeType) {
    this.edgeType = edgeType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeIdType)) {
        return false;
    }
    EdgeIdType o = (EdgeIdType) other;
    return edgeType.equals(o.edgeType);
  }
  
  @Override
  public int hashCode() {
    return 2 * edgeType.hashCode();
  }
}
