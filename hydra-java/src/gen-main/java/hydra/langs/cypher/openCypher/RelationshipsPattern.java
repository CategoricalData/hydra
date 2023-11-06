package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RelationshipsPattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipsPattern");
  
  public final hydra.langs.cypher.openCypher.NodePattern pattern;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chains;
  
  public RelationshipsPattern (hydra.langs.cypher.openCypher.NodePattern pattern, java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chains) {
    this.pattern = pattern;
    this.chains = chains;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipsPattern)) {
      return false;
    }
    RelationshipsPattern o = (RelationshipsPattern) (other);
    return pattern.equals(o.pattern) && chains.equals(o.chains);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * chains.hashCode();
  }
  
  public RelationshipsPattern withPattern(hydra.langs.cypher.openCypher.NodePattern pattern) {
    return new RelationshipsPattern(pattern, chains);
  }
  
  public RelationshipsPattern withChains(java.util.List<hydra.langs.cypher.openCypher.PatternElementChain> chains) {
    return new RelationshipsPattern(pattern, chains);
  }
}