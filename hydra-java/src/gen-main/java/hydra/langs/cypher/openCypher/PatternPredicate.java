package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PatternPredicate implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternPredicate");
  
  public final hydra.langs.cypher.openCypher.RelationshipsPattern value;
  
  public PatternPredicate (hydra.langs.cypher.openCypher.RelationshipsPattern value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternPredicate)) {
      return false;
    }
    PatternPredicate o = (PatternPredicate) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}