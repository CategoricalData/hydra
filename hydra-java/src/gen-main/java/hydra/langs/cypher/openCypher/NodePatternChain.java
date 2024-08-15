// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NodePatternChain implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NodePatternChain");
  
  public static final hydra.core.Name FIELD_NAME_NODE_PATTERN = new hydra.core.Name("nodePattern");
  
  public static final hydra.core.Name FIELD_NAME_CHAIN = new hydra.core.Name("chain");
  
  public final hydra.langs.cypher.openCypher.NodePattern nodePattern;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain;
  
  public NodePatternChain (hydra.langs.cypher.openCypher.NodePattern nodePattern, java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain) {
    java.util.Objects.requireNonNull((nodePattern));
    java.util.Objects.requireNonNull((chain));
    this.nodePattern = nodePattern;
    this.chain = chain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePatternChain)) {
      return false;
    }
    NodePatternChain o = (NodePatternChain) (other);
    return nodePattern.equals(o.nodePattern) && chain.equals(o.chain);
  }
  
  @Override
  public int hashCode() {
    return 2 * nodePattern.hashCode() + 3 * chain.hashCode();
  }
  
  public NodePatternChain withNodePattern(hydra.langs.cypher.openCypher.NodePattern nodePattern) {
    java.util.Objects.requireNonNull((nodePattern));
    return new NodePatternChain(nodePattern, chain);
  }
  
  public NodePatternChain withChain(java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain) {
    java.util.Objects.requireNonNull((chain));
    return new NodePatternChain(nodePattern, chain);
  }
}