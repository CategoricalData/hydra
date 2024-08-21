// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A vertex
 */
public class Vertex<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/pg/model.Vertex");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  /**
   * The label of the vertex
   */
  public final hydra.pg.model.VertexLabel label;
  
  /**
   * The unique identifier of the vertex
   */
  public final V id;
  
  /**
   * A key/value map of vertex properties
   */
  public final java.util.Map<hydra.pg.model.PropertyKey, V> properties;
  
  public Vertex (hydra.pg.model.VertexLabel label, V id, java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((properties));
    this.label = label;
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return label.equals(o.label) && id.equals(o.id) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * properties.hashCode();
  }
  
  public Vertex withLabel(hydra.pg.model.VertexLabel label) {
    java.util.Objects.requireNonNull((label));
    return new Vertex(label, id, properties);
  }
  
  public Vertex withId(V id) {
    java.util.Objects.requireNonNull((id));
    return new Vertex(label, id, properties);
  }
  
  public Vertex withProperties(java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    java.util.Objects.requireNonNull((properties));
    return new Vertex(label, id, properties);
  }
}