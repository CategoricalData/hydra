// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PatternWhere implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternWhere");
  
  public final hydra.langs.cypher.openCypher.Pattern pattern;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where;
  
  public PatternWhere (hydra.langs.cypher.openCypher.Pattern pattern, hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
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
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    return new PatternWhere(pattern, where);
  }
  
  public PatternWhere withWhere(hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    return new PatternWhere(pattern, where);
  }
}