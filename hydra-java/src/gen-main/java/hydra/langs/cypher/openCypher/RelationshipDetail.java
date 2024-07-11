// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RelationshipDetail implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipDetail");
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.RelationshipTypes> types;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.RangeLiteral> range;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Properties> properties;
  
  public RelationshipDetail (hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable, hydra.util.Opt<hydra.langs.cypher.openCypher.RelationshipTypes> types, hydra.util.Opt<hydra.langs.cypher.openCypher.RangeLiteral> range, hydra.util.Opt<hydra.langs.cypher.openCypher.Properties> properties) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    if (types == null) {
      throw new IllegalArgumentException("null value for 'types' argument");
    }
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    if (properties == null) {
      throw new IllegalArgumentException("null value for 'properties' argument");
    }
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
    RelationshipDetail o = (RelationshipDetail) (other);
    return variable.equals(o.variable) && types.equals(o.types) && range.equals(o.range) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * types.hashCode() + 5 * range.hashCode() + 7 * properties.hashCode();
  }
  
  public RelationshipDetail withVariable(hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withTypes(hydra.util.Opt<hydra.langs.cypher.openCypher.RelationshipTypes> types) {
    if (types == null) {
      throw new IllegalArgumentException("null value for 'types' argument");
    }
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withRange(hydra.util.Opt<hydra.langs.cypher.openCypher.RangeLiteral> range) {
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withProperties(hydra.util.Opt<hydra.langs.cypher.openCypher.Properties> properties) {
    if (properties == null) {
      throw new IllegalArgumentException("null value for 'properties' argument");
    }
    return new RelationshipDetail(variable, types, range, properties);
  }
}