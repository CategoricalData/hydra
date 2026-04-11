// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PatternWhere implements Serializable, Comparable<PatternWhere> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternWhere");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name WHERE = new hydra.core.Name("where");

  public final hydra.ext.cypher.openCypher.Pattern pattern;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where;

  public PatternWhere (hydra.ext.cypher.openCypher.Pattern pattern, hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    this.pattern = pattern;
    this.where = where;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternWhere)) {
      return false;
    }
    PatternWhere o = (PatternWhere) other;
    return java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.where,
      o.where);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pattern) + 3 * java.util.Objects.hashCode(where);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PatternWhere other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pattern,
      other.pattern);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      where,
      other.where);
  }

  public PatternWhere withPattern(hydra.ext.cypher.openCypher.Pattern pattern) {
    return new PatternWhere(pattern, where);
  }

  public PatternWhere withWhere(hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    return new PatternWhere(pattern, where);
  }
}
