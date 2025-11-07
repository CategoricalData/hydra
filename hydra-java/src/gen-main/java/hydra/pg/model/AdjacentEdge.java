// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

/**
 * An edge which is adjacent to a given vertex. Only the other endpoint of the edge is provided.
 */
public class AdjacentEdge<V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.AdjacentEdge");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  /**
   * The label of the edge
   */
  public final hydra.pg.model.EdgeLabel label;
  
  /**
   * The unique identifier of the edge
   */
  public final V id;
  
  /**
   * The id of the other vertex adjacent to the edge
   */
  public final V vertex;
  
  /**
   * A key/value map of edge properties
   */
  public final java.util.Map<hydra.pg.model.PropertyKey, V> properties;
  
  public AdjacentEdge (hydra.pg.model.EdgeLabel label, V id, V vertex, java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((vertex));
    java.util.Objects.requireNonNull((properties));
    this.label = label;
    this.id = id;
    this.vertex = vertex;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdjacentEdge)) {
      return false;
    }
    AdjacentEdge o = (AdjacentEdge) (other);
    return label.equals(o.label) && id.equals(o.id) && vertex.equals(o.vertex) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * vertex.hashCode() + 7 * properties.hashCode();
  }
  
  public AdjacentEdge withLabel(hydra.pg.model.EdgeLabel label) {
    java.util.Objects.requireNonNull((label));
    return new AdjacentEdge(label, id, vertex, properties);
  }
  
  public AdjacentEdge withId(V id) {
    java.util.Objects.requireNonNull((id));
    return new AdjacentEdge(label, id, vertex, properties);
  }
  
  public AdjacentEdge withVertex(V vertex) {
    java.util.Objects.requireNonNull((vertex));
    return new AdjacentEdge(label, id, vertex, properties);
  }
  
  public AdjacentEdge withProperties(java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    java.util.Objects.requireNonNull((properties));
    return new AdjacentEdge(label, id, vertex, properties);
  }
}
