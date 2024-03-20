package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class SelectQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.SelectQuery");
  
  public final Boolean distinct;
  
  public final hydra.langs.tinkerpop.queries.Projections projection;
  
  public SelectQuery (Boolean distinct, hydra.langs.tinkerpop.queries.Projections projection) {
    this.distinct = distinct;
    this.projection = projection;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectQuery)) {
      return false;
    }
    SelectQuery o = (SelectQuery) (other);
    return distinct.equals(o.distinct) && projection.equals(o.projection);
  }
  
  @Override
  public int hashCode() {
    return 2 * distinct.hashCode() + 3 * projection.hashCode();
  }
  
  public SelectQuery withDistinct(Boolean distinct) {
    return new SelectQuery(distinct, projection);
  }
  
  public SelectQuery withProjection(hydra.langs.tinkerpop.queries.Projections projection) {
    return new SelectQuery(distinct, projection);
  }
}