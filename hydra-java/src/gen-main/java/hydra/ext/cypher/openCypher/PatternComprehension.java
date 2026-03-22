// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PatternComprehension implements Serializable, Comparable<PatternComprehension> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternComprehension");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name WHERE = new hydra.core.Name("where");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable;

  public final hydra.ext.cypher.openCypher.RelationshipsPattern pattern;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where;

  public final hydra.ext.cypher.openCypher.Expression right;

  public PatternComprehension (hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable, hydra.ext.cypher.openCypher.RelationshipsPattern pattern, hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where, hydra.ext.cypher.openCypher.Expression right) {
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
    PatternComprehension o = (PatternComprehension) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.where,
      o.where) && java.util.Objects.equals(
      this.right,
      o.right);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(pattern) + 5 * java.util.Objects.hashCode(where) + 7 * java.util.Objects.hashCode(right);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PatternComprehension other) {
    int cmp = 0;
    cmp = ((Comparable) variable).compareTo(other.variable);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) pattern).compareTo(other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) where).compareTo(other.where);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) right).compareTo(other.right);
  }

  public PatternComprehension withVariable(hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable) {
    return new PatternComprehension(variable, pattern, where, right);
  }

  public PatternComprehension withPattern(hydra.ext.cypher.openCypher.RelationshipsPattern pattern) {
    return new PatternComprehension(variable, pattern, where, right);
  }

  public PatternComprehension withWhere(hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    return new PatternComprehension(variable, pattern, where, right);
  }

  public PatternComprehension withRight(hydra.ext.cypher.openCypher.Expression right) {
    return new PatternComprehension(variable, pattern, where, right);
  }
}
