package hydra.ext.tinkerpop.typed;

/**
 * The type of a reference to an edge by id
 */
public class EdgeIdType {
  /**
   * The type of a reference to an edge by id
   */
  public final EdgeType value;
  
  public EdgeIdType (EdgeType value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeIdType)) {
      return false;
    }
    EdgeIdType o = (EdgeIdType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}