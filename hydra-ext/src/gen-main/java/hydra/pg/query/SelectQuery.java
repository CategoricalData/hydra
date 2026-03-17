// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class SelectQuery implements Serializable, Comparable<SelectQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.SelectQuery");

  public static final hydra.core.Name DISTINCT = new hydra.core.Name("distinct");

  public static final hydra.core.Name PROJECTION = new hydra.core.Name("projection");

  public final Boolean distinct;

  public final hydra.pg.query.Projections projection;

  public SelectQuery (Boolean distinct, hydra.pg.query.Projections projection) {
    this.distinct = distinct;
    this.projection = projection;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelectQuery)) {
      return false;
    }
    SelectQuery o = (SelectQuery) other;
    return java.util.Objects.equals(
      this.distinct,
      o.distinct) && java.util.Objects.equals(
      this.projection,
      o.projection);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(distinct) + 3 * java.util.Objects.hashCode(projection);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SelectQuery other) {
    int cmp = 0;
    cmp = ((Comparable) distinct).compareTo(other.distinct);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) projection).compareTo(other.projection);
  }

  public SelectQuery withDistinct(Boolean distinct) {
    return new SelectQuery(distinct, projection);
  }

  public SelectQuery withProjection(hydra.pg.query.Projections projection) {
    return new SelectQuery(distinct, projection);
  }
}
