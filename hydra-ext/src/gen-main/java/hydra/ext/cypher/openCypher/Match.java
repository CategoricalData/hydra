// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Match implements Serializable, Comparable<Match> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Match");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name WHERE = new hydra.core.Name("where");

  public final Boolean optional;

  public final hydra.ext.cypher.openCypher.Pattern pattern;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where;

  public Match (Boolean optional, hydra.ext.cypher.openCypher.Pattern pattern, hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    this.optional = optional;
    this.pattern = pattern;
    this.where = where;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Match)) {
      return false;
    }
    Match o = (Match) other;
    return java.util.Objects.equals(
      this.optional,
      o.optional) && java.util.Objects.equals(
      this.pattern,
      o.pattern) && java.util.Objects.equals(
      this.where,
      o.where);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(optional) + 3 * java.util.Objects.hashCode(pattern) + 5 * java.util.Objects.hashCode(where);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Match other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      optional,
      other.optional);
    if (cmp != 0) {
      return cmp;
    }
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

  public Match withOptional(Boolean optional) {
    return new Match(optional, pattern, where);
  }

  public Match withPattern(hydra.ext.cypher.openCypher.Pattern pattern) {
    return new Match(optional, pattern, where);
  }

  public Match withWhere(hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    return new Match(optional, pattern, where);
  }
}
