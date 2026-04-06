// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class MatchQuery implements Serializable, Comparable<MatchQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.MatchQuery");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name WHERE = new hydra.core.Name("where");

  public final Boolean optional;

  public final java.util.List<hydra.pg.query.Projection> pattern;

  public final hydra.util.Maybe<hydra.pg.query.Expression> where;

  public MatchQuery (Boolean optional, java.util.List<hydra.pg.query.Projection> pattern, hydra.util.Maybe<hydra.pg.query.Expression> where) {
    this.optional = optional;
    this.pattern = pattern;
    this.where = where;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MatchQuery)) {
      return false;
    }
    MatchQuery o = (MatchQuery) other;
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
  public int compareTo(MatchQuery other) {
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

  public MatchQuery withOptional(Boolean optional) {
    return new MatchQuery(optional, pattern, where);
  }

  public MatchQuery withPattern(java.util.List<hydra.pg.query.Projection> pattern) {
    return new MatchQuery(optional, pattern, where);
  }

  public MatchQuery withWhere(hydra.util.Maybe<hydra.pg.query.Expression> where) {
    return new MatchQuery(optional, pattern, where);
  }
}
