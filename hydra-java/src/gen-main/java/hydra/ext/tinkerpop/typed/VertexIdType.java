package hydra.ext.tinkerpop.typed;

/**
 * The type of a reference to a vertex by id
 */
public class VertexIdType {
  /**
   * The type of a reference to a vertex by id
   */
  public final hydra.ext.tinkerpop.typed.VertexType value;
  
  public VertexIdType (hydra.ext.tinkerpop.typed.VertexType value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexIdType)) {
      return false;
    }
    VertexIdType o = (VertexIdType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}