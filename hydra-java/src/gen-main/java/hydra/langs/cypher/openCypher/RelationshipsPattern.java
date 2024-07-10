// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RelationshipsPattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipsPattern");
  
  public final hydra.langs.cypher.openCypher.NodePattern nodePattern;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain;
  
  public RelationshipsPattern (hydra.langs.cypher.openCypher.NodePattern nodePattern, java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain) {
    if (nodePattern == null) {
      throw new IllegalArgumentException("null value for 'nodePattern' argument");
    }
    if (chain == null) {
      throw new IllegalArgumentException("null value for 'chain' argument");
    }
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
  
  public RelationshipsPattern withNodePattern(hydra.langs.cypher.openCypher.NodePattern nodePattern) {
    if (nodePattern == null) {
      throw new IllegalArgumentException("null value for 'nodePattern' argument");
    }
    return new RelationshipsPattern(nodePattern, chain);
  }
  
  public RelationshipsPattern withChain(java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain) {
    if (chain == null) {
      throw new IllegalArgumentException("null value for 'chain' argument");
    }
    return new RelationshipsPattern(nodePattern, chain);
  }
}