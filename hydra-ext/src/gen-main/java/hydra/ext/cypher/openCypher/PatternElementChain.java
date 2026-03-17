// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PatternElementChain implements Serializable, Comparable<PatternElementChain> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternElementChain");

  public static final hydra.core.Name RELATIONSHIP = new hydra.core.Name("relationship");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public final hydra.ext.cypher.openCypher.RelationshipPattern relationship;

  public final hydra.ext.cypher.openCypher.NodePattern node;

  public PatternElementChain (hydra.ext.cypher.openCypher.RelationshipPattern relationship, hydra.ext.cypher.openCypher.NodePattern node) {
    this.relationship = relationship;
    this.node = node;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternElementChain)) {
      return false;
    }
    PatternElementChain o = (PatternElementChain) other;
    return java.util.Objects.equals(
      this.relationship,
      o.relationship) && java.util.Objects.equals(
      this.node,
      o.node);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(relationship) + 3 * java.util.Objects.hashCode(node);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PatternElementChain other) {
    int cmp = 0;
    cmp = ((Comparable) relationship).compareTo(other.relationship);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) node).compareTo(other.node);
  }

  public PatternElementChain withRelationship(hydra.ext.cypher.openCypher.RelationshipPattern relationship) {
    return new PatternElementChain(relationship, node);
  }

  public PatternElementChain withNode(hydra.ext.cypher.openCypher.NodePattern node) {
    return new PatternElementChain(relationship, node);
  }
}
