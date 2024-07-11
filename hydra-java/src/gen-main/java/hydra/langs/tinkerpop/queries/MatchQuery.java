// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class MatchQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.MatchQuery");
  
  public final Boolean optional;
  
  public final java.util.List<hydra.langs.tinkerpop.queries.Projection> pattern;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.queries.Expression> where;
  
  public MatchQuery (Boolean optional, java.util.List<hydra.langs.tinkerpop.queries.Projection> pattern, hydra.util.Opt<hydra.langs.tinkerpop.queries.Expression> where) {
    java.util.Objects.requireNonNull((optional));
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((where));
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
    java.util.Objects.requireNonNull((optional));
    return new MatchQuery(optional, pattern, where);
  }
  
  public MatchQuery withPattern(java.util.List<hydra.langs.tinkerpop.queries.Projection> pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new MatchQuery(optional, pattern, where);
  }
  
  public MatchQuery withWhere(hydra.util.Opt<hydra.langs.tinkerpop.queries.Expression> where) {
    java.util.Objects.requireNonNull((where));
    return new MatchQuery(optional, pattern, where);
  }
}