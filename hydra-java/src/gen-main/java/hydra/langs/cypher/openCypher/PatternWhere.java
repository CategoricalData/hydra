// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PatternWhere implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternWhere");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_WHERE = new hydra.core.Name("where");
  
  public final hydra.langs.cypher.openCypher.Pattern pattern;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where;
  
  public PatternWhere (hydra.langs.cypher.openCypher.Pattern pattern, hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((where));
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
    java.util.Objects.requireNonNull((pattern));
    return new PatternWhere(pattern, where);
  }
  
  public PatternWhere withWhere(hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((where));
    return new PatternWhere(pattern, where);
  }
}