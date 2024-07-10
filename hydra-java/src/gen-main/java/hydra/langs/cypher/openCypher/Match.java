// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Match implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Match");
  
  public final Boolean optional;
  
  public final hydra.langs.cypher.openCypher.Pattern pattern;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Where> where;
  
  public Match (Boolean optional, hydra.langs.cypher.openCypher.Pattern pattern, java.util.Optional<hydra.langs.cypher.openCypher.Where> where) {
    if (optional == null) {
      throw new IllegalArgumentException("null value for 'optional' argument");
    }
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    this.optional = optional;
    this.pattern = pattern;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Match)) {
      return false;
    }
    Match o = (Match) (other);
    return optional.equals(o.optional) && pattern.equals(o.pattern) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * optional.hashCode() + 3 * pattern.hashCode() + 5 * where.hashCode();
  }
  
  public Match withOptional(Boolean optional) {
    if (optional == null) {
      throw new IllegalArgumentException("null value for 'optional' argument");
    }
    return new Match(optional, pattern, where);
  }
  
  public Match withPattern(hydra.langs.cypher.openCypher.Pattern pattern) {
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    return new Match(optional, pattern, where);
  }
  
  public Match withWhere(java.util.Optional<hydra.langs.cypher.openCypher.Where> where) {
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    return new Match(optional, pattern, where);
  }
}