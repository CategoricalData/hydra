package hydra.ext.tinkerpop.v3;

import hydra.core.Literal;

/**
 * A literal value representing a vertex id
 */
public class VertexId {
  public final hydra.core.Literal literal;
  
  /**
   * Constructs an immutable VertexId object
   */
  public VertexId(hydra.core.Literal literal) {
    this.literal = literal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexId)) {
        return false;
    }
    VertexId o = (VertexId) other;
    return literal.equals(o.literal);
  }
  
  @Override
  public int hashCode() {
    return 2 * literal.hashCode();
  }
}
