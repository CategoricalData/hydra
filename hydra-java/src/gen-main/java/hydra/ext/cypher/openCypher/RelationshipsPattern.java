// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RelationshipsPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.RelationshipsPattern");
  
  public static final hydra.core.Name FIELD_NAME_NODE_PATTERN = new hydra.core.Name("nodePattern");
  
  public static final hydra.core.Name FIELD_NAME_CHAIN = new hydra.core.Name("chain");
  
  public final hydra.ext.cypher.openCypher.NodePattern nodePattern;
  
  public final java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain;
  
  public RelationshipsPattern (hydra.ext.cypher.openCypher.NodePattern nodePattern, java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain) {
    java.util.Objects.requireNonNull((nodePattern));
    java.util.Objects.requireNonNull((chain));
    this.nodePattern = nodePattern;
    this.chain = chain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipsPattern)) {
      return false;
    }
    RelationshipsPattern o = (RelationshipsPattern) (other);
    return nodePattern.equals(o.nodePattern) && chain.equals(o.chain);
  }
  
  @Override
  public int hashCode() {
    return 2 * nodePattern.hashCode() + 3 * chain.hashCode();
  }
  
  public RelationshipsPattern withNodePattern(hydra.ext.cypher.openCypher.NodePattern nodePattern) {
    java.util.Objects.requireNonNull((nodePattern));
    return new RelationshipsPattern(nodePattern, chain);
  }
  
  public RelationshipsPattern withChain(java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain) {
    java.util.Objects.requireNonNull((chain));
    return new RelationshipsPattern(nodePattern, chain);
  }
}
