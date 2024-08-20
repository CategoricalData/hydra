// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.queries;

import java.io.Serializable;

public class EdgeProjectionPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/queries.EdgeProjectionPattern");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTION = new hydra.core.Name("direction");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX = new hydra.core.Name("vertex");
  
  public final hydra.ext.tinkerpop.propertyGraph.Direction direction;
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.propertyGraph.EdgeLabel> label;
  
  public final java.util.List<hydra.ext.tinkerpop.queries.PropertyPattern> properties;
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.queries.VertexPattern> vertex;
  
  public EdgeProjectionPattern (hydra.ext.tinkerpop.propertyGraph.Direction direction, hydra.util.Opt<hydra.ext.tinkerpop.propertyGraph.EdgeLabel> label, java.util.List<hydra.ext.tinkerpop.queries.PropertyPattern> properties, hydra.util.Opt<hydra.ext.tinkerpop.queries.VertexPattern> vertex) {
    java.util.Objects.requireNonNull((direction));
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((vertex));
    this.direction = direction;
    this.label = label;
    this.properties = properties;
    this.vertex = vertex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeProjectionPattern)) {
      return false;
    }
    EdgeProjectionPattern o = (EdgeProjectionPattern) (other);
    return direction.equals(o.direction) && label.equals(o.label) && properties.equals(o.properties) && vertex.equals(o.vertex);
  }
  
  @Override
  public int hashCode() {
    return 2 * direction.hashCode() + 3 * label.hashCode() + 5 * properties.hashCode() + 7 * vertex.hashCode();
  }
  
  public EdgeProjectionPattern withDirection(hydra.ext.tinkerpop.propertyGraph.Direction direction) {
    java.util.Objects.requireNonNull((direction));
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }
  
  public EdgeProjectionPattern withLabel(hydra.util.Opt<hydra.ext.tinkerpop.propertyGraph.EdgeLabel> label) {
    java.util.Objects.requireNonNull((label));
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }
  
  public EdgeProjectionPattern withProperties(java.util.List<hydra.ext.tinkerpop.queries.PropertyPattern> properties) {
    java.util.Objects.requireNonNull((properties));
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }
  
  public EdgeProjectionPattern withVertex(hydra.util.Opt<hydra.ext.tinkerpop.queries.VertexPattern> vertex) {
    java.util.Objects.requireNonNull((vertex));
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }
}
