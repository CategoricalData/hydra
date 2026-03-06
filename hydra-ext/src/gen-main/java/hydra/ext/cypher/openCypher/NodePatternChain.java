// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class NodePatternChain implements Serializable, Comparable<NodePatternChain> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.NodePatternChain");
  
  public static final hydra.core.Name NODE_PATTERN = new hydra.core.Name("nodePattern");
  
  public static final hydra.core.Name CHAIN = new hydra.core.Name("chain");
  
  public final hydra.ext.cypher.openCypher.NodePattern nodePattern;
  
  public final java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain;
  
  public NodePatternChain (hydra.ext.cypher.openCypher.NodePattern nodePattern, java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain) {
    this.nodePattern = nodePattern;
    this.chain = chain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePatternChain)) {
      return false;
    }
    NodePatternChain o = (NodePatternChain) other;
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
  public int compareTo(NodePatternChain other) {
    int cmp = 0;
    cmp = ((Comparable) nodePattern).compareTo(other.nodePattern);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      chain.hashCode(),
      other.chain.hashCode());
  }
  
  public NodePatternChain withNodePattern(hydra.ext.cypher.openCypher.NodePattern nodePattern) {
    return new NodePatternChain(nodePattern, chain);
  }
  
  public NodePatternChain withChain(java.util.List<hydra.ext.cypher.openCypher.PatternElementChain> chain) {
    return new NodePatternChain(nodePattern, chain);
  }
}
