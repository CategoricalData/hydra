// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PatternPart implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternPart");
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable;
  
  public final hydra.langs.cypher.openCypher.AnonymousPatternPart pattern;
  
  public PatternPart (hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable, hydra.langs.cypher.openCypher.AnonymousPatternPart pattern) {
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
  
  public PatternPart withVariable(hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable) {
    java.util.Objects.requireNonNull((variable));
    return new PatternPart(variable, pattern);
  }
  
  public PatternPart withPattern(hydra.langs.cypher.openCypher.AnonymousPatternPart pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new PatternPart(variable, pattern);
  }
}