package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RelationshipDetail implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipDetail");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable;
  
  public final java.util.List<hydra.langs.cypher.openCypher.RelTypeName> types;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.RangeLiteral> range;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Properties> properties;
  
  public RelationshipDetail (java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable, java.util.List<hydra.langs.cypher.openCypher.RelTypeName> types, java.util.Optional<hydra.langs.cypher.openCypher.RangeLiteral> range, java.util.Optional<hydra.langs.cypher.openCypher.Properties> properties) {
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
  
  public RelationshipDetail withVariable(java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable) {
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withTypes(java.util.List<hydra.langs.cypher.openCypher.RelTypeName> types) {
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withRange(java.util.Optional<hydra.langs.cypher.openCypher.RangeLiteral> range) {
    return new RelationshipDetail(variable, types, range, properties);
  }
  
  public RelationshipDetail withProperties(java.util.Optional<hydra.langs.cypher.openCypher.Properties> properties) {
    return new RelationshipDetail(variable, types, range, properties);
  }
}