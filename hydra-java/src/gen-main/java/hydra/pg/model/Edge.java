// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * An edge
 */
public class Edge<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.Edge");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_OUT = new hydra.core.Name("out");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
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
   * The id of the out-vertex (tail) of the edge
   */
  public final V out;
  
  /**
   * The id of the in-vertex (head) of the edge
   */
  public final V in;
  
  /**
   * A key/value map of edge properties
   */
  public final java.util.Map<hydra.pg.model.PropertyKey, V> properties;
  
  public Edge (hydra.pg.model.EdgeLabel label, V id, V out, V in, java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
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
    if (!(other instanceof Edge)) {
      return false;
    }
    Edge o = (Edge) (other);
    return label.equals(o.label) && id.equals(o.id) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * out.hashCode() + 7 * in.hashCode() + 11 * properties.hashCode();
  }
  
  public Edge withLabel(hydra.pg.model.EdgeLabel label) {
    java.util.Objects.requireNonNull((label));
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withId(V id) {
    java.util.Objects.requireNonNull((id));
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withOut(V out) {
    java.util.Objects.requireNonNull((out));
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withIn(V in) {
    java.util.Objects.requireNonNull((in));
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withProperties(java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    java.util.Objects.requireNonNull((properties));
    return new Edge(label, id, out, in, properties);
  }
}