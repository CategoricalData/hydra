// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class With implements Serializable, Comparable<With> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.With");

  public static final hydra.core.Name PROJECTION = new hydra.core.Name("projection");

  public static final hydra.core.Name WHERE = new hydra.core.Name("where");

  public final hydra.cypher.openCypher.ProjectionBody projection;

  public final hydra.util.Maybe<hydra.cypher.openCypher.Where> where;

  public With (hydra.cypher.openCypher.ProjectionBody projection, hydra.util.Maybe<hydra.cypher.openCypher.Where> where) {
    this.projection = projection;
    this.where = where;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof With)) {
      return false;
    }
    With o = (With) other;
    return java.util.Objects.equals(
      this.projection,
      o.projection) && java.util.Objects.equals(
      this.where,
      o.where);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(projection) + 3 * java.util.Objects.hashCode(where);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(With other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      projection,
      other.projection);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      where,
      other.where);
  }

  public With withProjection(hydra.cypher.openCypher.ProjectionBody projection) {
    return new With(projection, where);
  }

  public With withWhere(hydra.util.Maybe<hydra.cypher.openCypher.Where> where) {
    return new With(projection, where);
  }
}
