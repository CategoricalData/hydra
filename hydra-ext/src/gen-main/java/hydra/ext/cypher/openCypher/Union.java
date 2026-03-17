// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Union implements Serializable, Comparable<Union> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Union");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  public static final hydra.core.Name QUERY = new hydra.core.Name("query");

  public final Boolean all;

  public final hydra.ext.cypher.openCypher.SingleQuery query;

  public Union (Boolean all, hydra.ext.cypher.openCypher.SingleQuery query) {
    this.all = all;
    this.query = query;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Union)) {
      return false;
    }
    Union o = (Union) other;
    return java.util.Objects.equals(
      this.all,
      o.all) && java.util.Objects.equals(
      this.query,
      o.query);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(all) + 3 * java.util.Objects.hashCode(query);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Union other) {
    int cmp = 0;
    cmp = ((Comparable) all).compareTo(other.all);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) query).compareTo(other.query);
  }

  public Union withAll(Boolean all) {
    return new Union(all, query);
  }

  public Union withQuery(hydra.ext.cypher.openCypher.SingleQuery query) {
    return new Union(all, query);
  }
}
