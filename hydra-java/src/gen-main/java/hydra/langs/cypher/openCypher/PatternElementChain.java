// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PatternElementChain implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternElementChain");
  
  public final hydra.langs.cypher.openCypher.RelationshipPattern relationship;
  
  public final hydra.langs.cypher.openCypher.NodePattern node;
  
  public PatternElementChain (hydra.langs.cypher.openCypher.RelationshipPattern relationship, hydra.langs.cypher.openCypher.NodePattern node) {
    if (relationship == null) {
      throw new IllegalArgumentException("null value for 'relationship' argument");
    }
    if (node == null) {
      throw new IllegalArgumentException("null value for 'node' argument");
    }
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
  
  public PatternElementChain withRelationship(hydra.langs.cypher.openCypher.RelationshipPattern relationship) {
    if (relationship == null) {
      throw new IllegalArgumentException("null value for 'relationship' argument");
    }
    return new PatternElementChain(relationship, node);
  }
  
  public PatternElementChain withNode(hydra.langs.cypher.openCypher.NodePattern node) {
    if (node == null) {
      throw new IllegalArgumentException("null value for 'node' argument");
    }
    return new PatternElementChain(relationship, node);
  }
}