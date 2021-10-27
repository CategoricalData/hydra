package hydra.ext.tinkerpop.v3;

import hydra.core.AtomicValue;

/**
 * An atomic value representing a vertex id
 */
public class VertexId {
  public final hydra.core.AtomicValue atomicValue;
  
  /**
   * Constructs an immutable VertexId object
   */
  public VertexId(hydra.core.AtomicValue atomicValue) {
    this.atomicValue = atomicValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexId)) {
        return false;
    }
    VertexId o = (VertexId) other;
    return atomicValue.equals(o.atomicValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * atomicValue.hashCode();
  }
}
