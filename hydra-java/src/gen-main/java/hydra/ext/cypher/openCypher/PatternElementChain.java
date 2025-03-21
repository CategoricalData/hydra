// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PatternElementChain implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternElementChain");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIP = new hydra.core.Name("relationship");
  
  public static final hydra.core.Name FIELD_NAME_NODE = new hydra.core.Name("node");
  
  public final hydra.ext.cypher.openCypher.RelationshipPattern relationship;
  
  public final hydra.ext.cypher.openCypher.NodePattern node;
  
  public PatternElementChain (hydra.ext.cypher.openCypher.RelationshipPattern relationship, hydra.ext.cypher.openCypher.NodePattern node) {
    java.util.Objects.requireNonNull((relationship));
    java.util.Objects.requireNonNull((node));
    this.relationship = relationship;
    this.node = node;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternElementChain)) {
      return false;
    }
    PatternElementChain o = (PatternElementChain) (other);
    return relationship.equals(o.relationship) && node.equals(o.node);
  }
  
  @Override
  public int hashCode() {
    return 2 * relationship.hashCode() + 3 * node.hashCode();
  }
  
  public PatternElementChain withRelationship(hydra.ext.cypher.openCypher.RelationshipPattern relationship) {
    java.util.Objects.requireNonNull((relationship));
    return new PatternElementChain(relationship, node);
  }
  
  public PatternElementChain withNode(hydra.ext.cypher.openCypher.NodePattern node) {
    java.util.Objects.requireNonNull((node));
    return new PatternElementChain(relationship, node);
  }
}