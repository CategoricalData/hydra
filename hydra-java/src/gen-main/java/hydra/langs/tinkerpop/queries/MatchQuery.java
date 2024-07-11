// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class MatchQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.MatchQuery");
  
  public final Boolean optional;
  
  public final java.util.List<hydra.langs.tinkerpop.queries.Projection> pattern;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.queries.Expression> where;
  
  public MatchQuery (Boolean optional, java.util.List<hydra.langs.tinkerpop.queries.Projection> pattern, hydra.util.Opt<hydra.langs.tinkerpop.queries.Expression> where) {
    if (optional == null) {
      throw new IllegalArgumentException("null value for 'optional' argument");
    }
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    this.optional = optional;
    this.pattern = pattern;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MatchQuery)) {
      return false;
    }
    MatchQuery o = (MatchQuery) (other);
    return optional.equals(o.optional) && pattern.equals(o.pattern) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * optional.hashCode() + 3 * pattern.hashCode() + 5 * where.hashCode();
  }
  
  public MatchQuery withOptional(Boolean optional) {
    if (optional == null) {
      throw new IllegalArgumentException("null value for 'optional' argument");
    }
    return new MatchQuery(optional, pattern, where);
  }
  
  public MatchQuery withPattern(java.util.List<hydra.langs.tinkerpop.queries.Projection> pattern) {
    if (pattern == null) {
      throw new IllegalArgumentException("null value for 'pattern' argument");
    }
    return new MatchQuery(optional, pattern, where);
  }
  
  public MatchQuery withWhere(hydra.util.Opt<hydra.langs.tinkerpop.queries.Expression> where) {
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    return new MatchQuery(optional, pattern, where);
  }
}