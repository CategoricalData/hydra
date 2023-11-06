package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NodePatternExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NodePatternExpression");
  
  public final hydra.langs.cypher.openCypher.NodePattern pattern;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chains;
  
  public NodePatternExpression (hydra.langs.cypher.openCypher.NodePattern pattern, java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chains) {
    this.pattern = pattern;
    this.chains = chains;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePatternExpression)) {
      return false;
    }
    NodePatternExpression o = (NodePatternExpression) (other);
    return pattern.equals(o.pattern) && chains.equals(o.chains);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * chains.hashCode();
  }
  
  public NodePatternExpression withPattern(hydra.langs.cypher.openCypher.NodePattern pattern) {
    return new NodePatternExpression(pattern, chains);
  }
  
  public NodePatternExpression withChains(java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chains) {
    return new NodePatternExpression(pattern, chains);
  }
}