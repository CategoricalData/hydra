package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PatternComprehension implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternComprehension");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable;
  
  public final hydra.langs.cypher.openCypher.RelationshipsPattern pattern;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> where;
  
  public final hydra.langs.cypher.openCypher.Where right;
  
  public PatternComprehension (java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable, hydra.langs.cypher.openCypher.RelationshipsPattern pattern, java.util.Optional<hydra.langs.cypher.openCypher.Expression> where, hydra.langs.cypher.openCypher.Where right) {
    this.variable = variable;
    this.pattern = pattern;
    this.where = where;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternComprehension)) {
      return false;
    }
    PatternComprehension o = (PatternComprehension) (other);
    return variable.equals(o.variable) && pattern.equals(o.pattern) && where.equals(o.where) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * pattern.hashCode() + 5 * where.hashCode() + 7 * right.hashCode();
  }
  
  public PatternComprehension withVariable(java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable) {
    return new PatternComprehension(variable, pattern, where, right);
  }
  
  public PatternComprehension withPattern(hydra.langs.cypher.openCypher.RelationshipsPattern pattern) {
    return new PatternComprehension(variable, pattern, where, right);
  }
  
  public PatternComprehension withWhere(java.util.Optional<hydra.langs.cypher.openCypher.Expression> where) {
    return new PatternComprehension(variable, pattern, where, right);
  }
  
  public PatternComprehension withRight(hydra.langs.cypher.openCypher.Where right) {
    return new PatternComprehension(variable, pattern, where, right);
  }
}