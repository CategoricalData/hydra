// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

import java.io.Serializable;

/**
 * A mapping specification producing edges of a specified label.
 */
public class EdgeSpec implements Serializable, Comparable<EdgeSpec> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.mapping.EdgeSpec");
  
  public static final hydra.core.Name LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name OUT = new hydra.core.Name("out");
  
  public static final hydra.core.Name IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  /**
   * The label of the target edges, which must conform to the edge type associated with that label.
   */
  public final hydra.pg.model.EdgeLabel label;
  
  /**
   * A specification of the id of each target edge
   */
  public final hydra.pg.mapping.ValueSpec id;
  
  /**
   * A specification of the out-vertex reference of each target edge
   */
  public final hydra.pg.mapping.ValueSpec out;
  
  /**
   * A specification of the in-vertex reference of each target edge
   */
  public final hydra.pg.mapping.ValueSpec in;
  
  /**
   * Zero or more property specifications for each target edge
   */
  public final hydra.util.ConsList<hydra.pg.mapping.PropertySpec> properties;
  
  public EdgeSpec (hydra.pg.model.EdgeLabel label, hydra.pg.mapping.ValueSpec id, hydra.pg.mapping.ValueSpec out, hydra.pg.mapping.ValueSpec in, hydra.util.ConsList<hydra.pg.mapping.PropertySpec> properties) {
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
    EdgeSpec o = (EdgeSpec) other;
    return java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.out,
      o.out) && java.util.Objects.equals(
      this.in,
      o.in) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label) + 3 * java.util.Objects.hashCode(id) + 5 * java.util.Objects.hashCode(out) + 7 * java.util.Objects.hashCode(in) + 11 * java.util.Objects.hashCode(properties);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeSpec other) {
    int cmp = 0;
    cmp = ((Comparable) label).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) id).compareTo(other.id);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) out).compareTo(other.out);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) in).compareTo(other.in);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) properties).compareTo(other.properties);
  }
  
  public EdgeSpec withLabel(hydra.pg.model.EdgeLabel label) {
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withId(hydra.pg.mapping.ValueSpec id) {
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withOut(hydra.pg.mapping.ValueSpec out) {
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withIn(hydra.pg.mapping.ValueSpec in) {
    return new EdgeSpec(label, id, out, in, properties);
  }
  
  public EdgeSpec withProperties(hydra.util.ConsList<hydra.pg.mapping.PropertySpec> properties) {
    return new EdgeSpec(label, id, out, in, properties);
  }
}
