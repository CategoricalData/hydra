package hydra.langs.tinkerpop.typed;

/**
 * The type of a reference to an edge by id
 */
public class EdgeIdType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/typed.EdgeIdType");
  
  /**
   * The type of a reference to an edge by id
   */
  public final hydra.langs.tinkerpop.typed.EdgeType value;
  
  public EdgeIdType (hydra.langs.tinkerpop.typed.EdgeType value) {
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