// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;

public class PatternComprehension implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternComprehension");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_WHERE = new hydra.core.Name("where");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final Maybe<Variable> variable;
  
  public final hydra.ext.cypher.openCypher.RelationshipsPattern pattern;
  
  public final Maybe<Where> where;
  
  public final hydra.ext.cypher.openCypher.Expression right;
  
  public PatternComprehension (Maybe<Variable> variable, hydra.ext.cypher.openCypher.RelationshipsPattern pattern, Maybe<Where> where, hydra.ext.cypher.openCypher.Expression right) {
    java.util.Objects.requireNonNull((variable));
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((where));
    java.util.Objects.requireNonNull((right));
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
  
  public PatternComprehension withVariable(Maybe<Variable> variable) {
    java.util.Objects.requireNonNull((variable));
    return new PatternComprehension(variable, pattern, where, right);
  }
  
  public PatternComprehension withPattern(hydra.ext.cypher.openCypher.RelationshipsPattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new PatternComprehension(variable, pattern, where, right);
  }
  
  public PatternComprehension withWhere(Maybe<Where> where) {
    java.util.Objects.requireNonNull((where));
    return new PatternComprehension(variable, pattern, where, right);
  }
  
  public PatternComprehension withRight(hydra.ext.cypher.openCypher.Expression right) {
    java.util.Objects.requireNonNull((right));
    return new PatternComprehension(variable, pattern, where, right);
  }
}
