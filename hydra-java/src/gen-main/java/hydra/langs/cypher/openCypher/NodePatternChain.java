package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NodePatternChain implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NodePatternChain");
  
  public final hydra.langs.cypher.openCypher.NodePattern nodePattern;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain;
  
  public NodePatternChain (hydra.langs.cypher.openCypher.NodePattern nodePattern, java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain) {
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
    return new NodePatternChain(nodePattern, chain);
  }
  
  public NodePatternChain withChain(java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chain) {
    return new NodePatternChain(nodePattern, chain);
  }
}