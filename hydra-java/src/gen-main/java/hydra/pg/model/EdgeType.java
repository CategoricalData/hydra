// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The type of an edge
 */
public class EdgeType<T> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.EdgeType");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_OUT = new hydra.core.Name("out");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  /**
   * The label of any edge of this edge type
   */
  public final hydra.pg.model.EdgeLabel label;
  
  /**
   * The type of the id of any edge of this edge type
   */
  public final T id;
  
  /**
   * The label of the out-vertex (tail) of any edge of this edge type
   */
  public final hydra.pg.model.VertexLabel out;
  
  /**
   * The label of the in-vertex (head) of any edge of this edge type
   */
  public final hydra.pg.model.VertexLabel in;
  
  /**
   * A list of property types. The types are ordered for the sake of applications in which property order is significant.
   */
  public final java.util.List<hydra.pg.model.PropertyType<T>> properties;
  
  public EdgeType (hydra.pg.model.EdgeLabel label, T id, hydra.pg.model.VertexLabel out, hydra.pg.model.VertexLabel in, java.util.List<hydra.pg.model.PropertyType<T>> properties) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((out));
    java.util.Objects.requireNonNull((in));
    java.util.Objects.requireNonNull((properties));
    this.label = label;
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
    EdgeType o = (EdgeType) (other);
    return label.equals(o.label) && id.equals(o.id) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * out.hashCode() + 7 * in.hashCode() + 11 * properties.hashCode();
  }
  
  public EdgeType withLabel(hydra.pg.model.EdgeLabel label) {
    java.util.Objects.requireNonNull((label));
    return new EdgeType(label, id, out, in, properties);
  }
  
  public EdgeType withId(T id) {
    java.util.Objects.requireNonNull((id));
    return new EdgeType(label, id, out, in, properties);
  }
  
  public EdgeType withOut(hydra.pg.model.VertexLabel out) {
    java.util.Objects.requireNonNull((out));
    return new EdgeType(label, id, out, in, properties);
  }
  
  public EdgeType withIn(hydra.pg.model.VertexLabel in) {
    java.util.Objects.requireNonNull((in));
    return new EdgeType(label, id, out, in, properties);
  }
  
  public EdgeType withProperties(java.util.List<hydra.pg.model.PropertyType<T>> properties) {
    java.util.Objects.requireNonNull((properties));
    return new EdgeType(label, id, out, in, properties);
  }
}