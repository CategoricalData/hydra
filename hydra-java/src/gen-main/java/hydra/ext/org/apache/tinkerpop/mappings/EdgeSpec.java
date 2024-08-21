// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.mappings;

import java.io.Serializable;

/**
 * A mapping specification producing edges of a specified label.
 */
public class EdgeSpec implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/mappings.EdgeSpec");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_OUT = new hydra.core.Name("out");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  /**
   * The label of the target edges, which must conform to the edge type associated with that label.
   */
  public final hydra.ext.org.apache.tinkerpop.propertyGraph.EdgeLabel label;
  
  /**
   * A specification of the id of each target edge
   */
  public final hydra.ext.org.apache.tinkerpop.mappings.ValueSpec id;
  
  /**
   * A specification of the out-vertex reference of each target edge
   */
  public final hydra.ext.org.apache.tinkerpop.mappings.ValueSpec out;
  
  /**
   * A specification of the in-vertex reference of each target edge
   */
  public final hydra.ext.org.apache.tinkerpop.mappings.ValueSpec in;
  
  /**
   * Zero or more property specifications for each target edge
   */
  public final java.util.List<hydra.ext.org.apache.tinkerpop.mappings.PropertySpec> properties;
  
  public EdgeSpec (hydra.ext.org.apache.tinkerpop.propertyGraph.EdgeLabel label, hydra.ext.org.apache.tinkerpop.mappings.ValueSpec id, hydra.ext.org.apache.tinkerpop.mappings.ValueSpec out, hydra.ext.org.apache.tinkerpop.mappings.ValueSpec in, java.util.List<hydra.ext.org.apache.tinkerpop.mappings.PropertySpec> properties) {
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
    if (!(other instanceof EdgeSpec)) {
      return false;
    }
    EdgeSpec o = (EdgeSpec) (other);
    return label.equals(o.label) && id.equals(o.id) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * out.hashCode() + 7 * in.hashCode() + 11 * properties.hashCode();
  }
  
  public EdgeSpec withLabel(hydra.ext.org.apache.tinkerpop.propertyGraph.EdgeLabel label) {
    java.util.Objects.requireNonNull((label));
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withId(hydra.ext.org.apache.tinkerpop.mappings.ValueSpec id) {
    java.util.Objects.requireNonNull((id));
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withOut(hydra.ext.org.apache.tinkerpop.mappings.ValueSpec out) {
    java.util.Objects.requireNonNull((out));
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withIn(hydra.ext.org.apache.tinkerpop.mappings.ValueSpec in) {
    java.util.Objects.requireNonNull((in));
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withProperties(java.util.List<hydra.ext.org.apache.tinkerpop.mappings.PropertySpec> properties) {
    java.util.Objects.requireNonNull((properties));
    return new EdgeSpec(label, id, out, in, properties);
  }
}