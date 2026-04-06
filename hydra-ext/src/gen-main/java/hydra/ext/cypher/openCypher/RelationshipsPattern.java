// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RelationshipsPattern implements Serializable, Comparable<RelationshipsPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RelationshipsPattern");

  public static final hydra.core.Name NODE_PATTERN = new hydra.core.Name("nodePattern");

  public static final hydra.core.Name CHAIN = new hydra.core.Name("chain");

  public final hydra.ext.cypher.openCypher.NodePattern nodePattern;

  public final java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain;

  public RelationshipsPattern (hydra.ext.cypher.openCypher.NodePattern nodePattern, java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain) {
    this.nodePattern = nodePattern;
    this.chain = chain;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipsPattern)) {
      return false;
    }
    RelationshipsPattern o = (RelationshipsPattern) other;
    return java.util.Objects.equals(
      this.nodePattern,
      o.nodePattern) && java.util.Objects.equals(
      this.chain,
      o.chain);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(nodePattern) + 3 * java.util.Objects.hashCode(chain);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelationshipsPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      nodePattern,
      other.nodePattern);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      chain,
      other.chain);
  }

  public RelationshipsPattern withNodePattern(hydra.ext.cypher.openCypher.NodePattern nodePattern) {
    return new RelationshipsPattern(nodePattern, chain);
  }

  public RelationshipsPattern withChain(java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain) {
    return new RelationshipsPattern(nodePattern, chain);
  }
}
