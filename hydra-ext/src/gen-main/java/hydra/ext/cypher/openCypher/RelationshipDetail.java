// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RelationshipDetail implements Serializable, Comparable<RelationshipDetail> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RelationshipDetail");
  
  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name TYPES = new hydra.core.Name("types");
  
  public static final hydra.core.Name RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.RelationshipTypes> types;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.RangeLiteral> range;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Properties> properties;
  
  public RelationshipDetail (hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable, hydra.util.Maybe<hydra.ext.cypher.openCypher.RelationshipTypes> types, hydra.util.Maybe<hydra.ext.cypher.openCypher.RangeLiteral> range, hydra.util.Maybe<hydra.ext.cypher.openCypher.Properties> properties) {
    this.variable = variable;
    this.types = types;
    this.range = range;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipDetail)) {
      return false;
    }
    RelationshipDetail o = (RelationshipDetail) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.types,
      o.types) && java.util.Objects.equals(
      this.range,
      o.range) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(types) + 5 * java.util.Objects.hashCode(range) + 7 * java.util.Objects.hashCode(properties);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelationshipDetail other) {
    int cmp = 0;
    cmp = ((Comparable) variable).compareTo(other.variable);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) types).compareTo(other.types);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) range).compareTo(other.range);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) properties).compareTo(other.properties);
  }
  
  public RelationshipDetail withVariable(hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable) {
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withTypes(hydra.util.Maybe<hydra.ext.cypher.openCypher.RelationshipTypes> types) {
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withRange(hydra.util.Maybe<hydra.ext.cypher.openCypher.RangeLiteral> range) {
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withProperties(hydra.util.Maybe<hydra.ext.cypher.openCypher.Properties> properties) {
    return new RelationshipDetail(variable, types, range, properties);
  }
}
