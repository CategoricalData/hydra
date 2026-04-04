// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PatternPart implements Serializable, Comparable<PatternPart> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternPart");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable;

  public final hydra.ext.cypher.openCypher.AnonymousPatternPart pattern;

  public PatternPart (hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable, hydra.ext.cypher.openCypher.AnonymousPatternPart pattern) {
    this.variable = variable;
    this.pattern = pattern;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternPart)) {
      return false;
    }
    PatternPart o = (PatternPart) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.pattern,
      o.pattern);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(pattern);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PatternPart other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variable,
      other.variable);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      pattern,
      other.pattern);
  }

  public PatternPart withVariable(hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable) {
    return new PatternPart(variable, pattern);
  }

  public PatternPart withPattern(hydra.ext.cypher.openCypher.AnonymousPatternPart pattern) {
    return new PatternPart(variable, pattern);
  }
}
