// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Match implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Match");
  
  public final Boolean optional;
  
  public final hydra.langs.cypher.openCypher.Pattern pattern;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where;
  
  public Match (Boolean optional, hydra.langs.cypher.openCypher.Pattern pattern, hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((optional));
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((where));
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
    java.util.Objects.requireNonNull((optional));
    return new Match(optional, pattern, where);
  }
  
  public Match withPattern(hydra.langs.cypher.openCypher.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new Match(optional, pattern, where);
  }
  
  public Match withWhere(hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((where));
    return new Match(optional, pattern, where);
  }
}