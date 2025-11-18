// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;

public class PatternPart implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternPart");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final Maybe<Variable> variable;
  
  public final hydra.ext.cypher.openCypher.AnonymousPatternPart pattern;
  
  public PatternPart (Maybe<Variable> variable, hydra.ext.cypher.openCypher.AnonymousPatternPart pattern) {
    java.util.Objects.requireNonNull((variable));
    java.util.Objects.requireNonNull((pattern));
    this.variable = variable;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternPart)) {
      return false;
    }
    PatternPart o = (PatternPart) (other);
    return variable.equals(o.variable) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * pattern.hashCode();
  }
  
  public PatternPart withVariable(Maybe<Variable> variable) {
    java.util.Objects.requireNonNull((variable));
    return new PatternPart(variable, pattern);
  }
  
  public PatternPart withPattern(hydra.ext.cypher.openCypher.AnonymousPatternPart pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new PatternPart(variable, pattern);
  }
}
