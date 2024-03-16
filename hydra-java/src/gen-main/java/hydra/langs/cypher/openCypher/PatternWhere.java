package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PatternWhere implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternWhere");
  
  public final hydra.langs.cypher.openCypher.Pattern pattern;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Where> where;
  
  public PatternWhere (hydra.langs.cypher.openCypher.Pattern pattern, java.util.Optional<hydra.langs.cypher.openCypher.Where> where) {
    this.pattern = pattern;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternWhere)) {
      return false;
    }
    PatternWhere o = (PatternWhere) (other);
    return pattern.equals(o.pattern) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * where.hashCode();
  }
  
  public PatternWhere withPattern(hydra.langs.cypher.openCypher.Pattern pattern) {
    return new PatternWhere(pattern, where);
  }
  
  public PatternWhere withWhere(java.util.Optional<hydra.langs.cypher.openCypher.Where> where) {
    return new PatternWhere(pattern, where);
  }
}