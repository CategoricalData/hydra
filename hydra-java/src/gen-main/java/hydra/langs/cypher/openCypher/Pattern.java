package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Pattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Pattern");
  
  public final java.util.List<hydra.langs.cypher.openCypher.PatternPart> parts;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> where;
  
  public Pattern (java.util.List<hydra.langs.cypher.openCypher.PatternPart> parts, java.util.Optional<hydra.langs.cypher.openCypher.Expression> where) {
    this.parts = parts;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern)) {
      return false;
    }
    Pattern o = (Pattern) (other);
    return parts.equals(o.parts) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * parts.hashCode() + 3 * where.hashCode();
  }
  
  public Pattern withParts(java.util.List<hydra.langs.cypher.openCypher.PatternPart> parts) {
    return new Pattern(parts, where);
  }
  
  public Pattern withWhere(java.util.Optional<hydra.langs.cypher.openCypher.Expression> where) {
    return new Pattern(parts, where);
  }
}