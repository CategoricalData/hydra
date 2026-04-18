// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class FilterExpression implements Serializable, Comparable<FilterExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.FilterExpression");

  public static final hydra.core.Name ID_IN_COLL = new hydra.core.Name("idInColl");

  public static final hydra.core.Name WHERE = new hydra.core.Name("where");

  public final hydra.cypher.openCypher.IdInColl idInColl;

  public final hydra.util.Maybe<hydra.cypher.openCypher.Where> where;

  public FilterExpression (hydra.cypher.openCypher.IdInColl idInColl, hydra.util.Maybe<hydra.cypher.openCypher.Where> where) {
    this.idInColl = idInColl;
    this.where = where;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FilterExpression)) {
      return false;
    }
    FilterExpression o = (FilterExpression) other;
    return java.util.Objects.equals(
      this.idInColl,
      o.idInColl) && java.util.Objects.equals(
      this.where,
      o.where);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(idInColl) + 3 * java.util.Objects.hashCode(where);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FilterExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      idInColl,
      other.idInColl);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      where,
      other.where);
  }

  public FilterExpression withIdInColl(hydra.cypher.openCypher.IdInColl idInColl) {
    return new FilterExpression(idInColl, where);
  }

  public FilterExpression withWhere(hydra.util.Maybe<hydra.cypher.openCypher.Where> where) {
    return new FilterExpression(idInColl, where);
  }
}
