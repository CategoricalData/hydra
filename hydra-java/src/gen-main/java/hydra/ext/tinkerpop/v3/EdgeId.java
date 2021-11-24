package hydra.ext.tinkerpop.v3;

import hydra.core.Literal;

/**
 * A literal value representing an edge id
 */
public class EdgeId {
  public final hydra.core.Literal literal;
  
  /**
   * Constructs an immutable EdgeId object
   */
  public EdgeId(hydra.core.Literal literal) {
    this.literal = literal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeId)) {
        return false;
    }
    EdgeId o = (EdgeId) other;
    return literal.equals(o.literal);
  }
  
  @Override
  public int hashCode() {
    return 2 * literal.hashCode();
  }
}
