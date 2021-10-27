package hydra.ext.tinkerpop.v3;

import hydra.core.AtomicType;

/**
 * The type of an edge, with characteristic id, out-vertex, in-vertex, and property types
 */
public class EdgeType {
  public final hydra.core.AtomicType id;
  
  public final hydra.ext.tinkerpop.v3.VertexIdType out;
  
  public final hydra.ext.tinkerpop.v3.VertexIdType in;
  
  public final java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Type> properties;
  
  /**
   * Constructs an immutable EdgeType object
   */
  public EdgeType(hydra.core.AtomicType id, hydra.ext.tinkerpop.v3.VertexIdType out, hydra.ext.tinkerpop.v3.VertexIdType in, java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Type> properties) {
    this.id = id;
    this.out = out;
    this.in = in;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeType)) {
        return false;
    }
    EdgeType o = (EdgeType) other;
    return id.equals(o.id)
        && out.equals(o.out)
        && in.equals(o.in)
        && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode()
        + 3 * out.hashCode()
        + 5 * in.hashCode()
        + 7 * properties.hashCode();
  }
  
  /**
   * Construct a new immutable EdgeType object in which id is overridden
   */
  public EdgeType withId(hydra.core.AtomicType id) {
    return new EdgeType(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable EdgeType object in which out is overridden
   */
  public EdgeType withOut(hydra.ext.tinkerpop.v3.VertexIdType out) {
    return new EdgeType(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable EdgeType object in which in is overridden
   */
  public EdgeType withIn(hydra.ext.tinkerpop.v3.VertexIdType in) {
    return new EdgeType(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable EdgeType object in which properties is overridden
   */
  public EdgeType withProperties(java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Type> properties) {
    return new EdgeType(id, out, in, properties);
  }
}
