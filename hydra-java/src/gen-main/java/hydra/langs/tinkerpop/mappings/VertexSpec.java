// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.mappings;

import java.io.Serializable;

/**
 * A mapping specification producing vertices of a specified label
 */
public class VertexSpec implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/mappings.VertexSpec");
  
  /**
   * The label of the target vertices, which must conform to the vertex type associated with that label.
   */
  public final hydra.langs.tinkerpop.propertyGraph.VertexLabel label;
  
  /**
   * A specification of the id of each target vertex
   */
  public final hydra.langs.tinkerpop.mappings.ValueSpec id;
  
  /**
   * Zero or more property specifications for each target vertex
   */
  public final java.util.List<hydra.langs.tinkerpop.mappings.PropertySpec> properties;
  
  public VertexSpec (hydra.langs.tinkerpop.propertyGraph.VertexLabel label, hydra.langs.tinkerpop.mappings.ValueSpec id, java.util.List<hydra.langs.tinkerpop.mappings.PropertySpec> properties) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((properties));
    this.label = label;
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexSpec)) {
      return false;
    }
    VertexSpec o = (VertexSpec) (other);
    return label.equals(o.label) && id.equals(o.id) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * properties.hashCode();
  }
  
  public VertexSpec withLabel(hydra.langs.tinkerpop.propertyGraph.VertexLabel label) {
    java.util.Objects.requireNonNull((label));
    return new VertexSpec(label, id, properties);
  }
  
  public VertexSpec withId(hydra.langs.tinkerpop.mappings.ValueSpec id) {
    java.util.Objects.requireNonNull((id));
    return new VertexSpec(label, id, properties);
  }
  
  public VertexSpec withProperties(java.util.List<hydra.langs.tinkerpop.mappings.PropertySpec> properties) {
    java.util.Objects.requireNonNull((properties));
    return new VertexSpec(label, id, properties);
  }
}