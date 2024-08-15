// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class SelectQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.SelectQuery");
  
  public static final hydra.core.Name FIELD_NAME_DISTINCT = new hydra.core.Name("distinct");
  
  public static final hydra.core.Name FIELD_NAME_PROJECTION = new hydra.core.Name("projection");
  
  public final Boolean distinct;
  
  public final hydra.langs.tinkerpop.queries.Projections projection;
  
  public SelectQuery (Boolean distinct, hydra.langs.tinkerpop.queries.Projections projection) {
    java.util.Objects.requireNonNull((distinct));
    java.util.Objects.requireNonNull((projection));
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
    java.util.Objects.requireNonNull((distinct));
    return new SelectQuery(distinct, projection);
  }
  
  public SelectQuery withProjection(hydra.langs.tinkerpop.queries.Projections projection) {
    java.util.Objects.requireNonNull((projection));
    return new SelectQuery(distinct, projection);
  }
}