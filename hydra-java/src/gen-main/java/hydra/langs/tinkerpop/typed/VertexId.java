package hydra.langs.tinkerpop.typed;

/**
 * A literal value representing a vertex id
 */
public class VertexId {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/typed.VertexId");
  
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