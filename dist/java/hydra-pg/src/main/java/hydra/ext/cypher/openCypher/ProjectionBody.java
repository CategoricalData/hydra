// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ProjectionBody implements Serializable, Comparable<ProjectionBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ProjectionBody");

  public static final hydra.core.Name DISTINCT = new hydra.core.Name("distinct");

  public static final hydra.core.Name PROJECTION_ITEMS = new hydra.core.Name("projectionItems");

  public static final hydra.core.Name ORDER = new hydra.core.Name("order");

  public static final hydra.core.Name SKIP = new hydra.core.Name("skip");

  public static final hydra.core.Name LIMIT = new hydra.core.Name("limit");

  public final Boolean distinct;

  public final hydra.ext.cypher.openCypher.ProjectionItems projectionItems;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Order> order;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Skip> skip;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Limit> limit;

  public ProjectionBody (Boolean distinct, hydra.ext.cypher.openCypher.ProjectionItems projectionItems, hydra.util.Maybe<hydra.ext.cypher.openCypher.Order> order, hydra.util.Maybe<hydra.ext.cypher.openCypher.Skip> skip, hydra.util.Maybe<hydra.ext.cypher.openCypher.Limit> limit) {
    this.distinct = distinct;
    this.projectionItems = projectionItems;
    this.order = order;
    this.skip = skip;
    this.limit = limit;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionBody)) {
      return false;
    }
    ProjectionBody o = (ProjectionBody) other;
    return java.util.Objects.equals(
      this.distinct,
      o.distinct) && java.util.Objects.equals(
      this.projectionItems,
      o.projectionItems) && java.util.Objects.equals(
      this.order,
      o.order) && java.util.Objects.equals(
      this.skip,
      o.skip) && java.util.Objects.equals(
      this.limit,
      o.limit);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(distinct) + 3 * java.util.Objects.hashCode(projectionItems) + 5 * java.util.Objects.hashCode(order) + 7 * java.util.Objects.hashCode(skip) + 11 * java.util.Objects.hashCode(limit);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProjectionBody other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      distinct,
      other.distinct);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      projectionItems,
      other.projectionItems);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      order,
      other.order);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      skip,
      other.skip);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      limit,
      other.limit);
  }

  public ProjectionBody withDistinct(Boolean distinct) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }

  public ProjectionBody withProjectionItems(hydra.ext.cypher.openCypher.ProjectionItems projectionItems) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }

  public ProjectionBody withOrder(hydra.util.Maybe<hydra.ext.cypher.openCypher.Order> order) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }

  public ProjectionBody withSkip(hydra.util.Maybe<hydra.ext.cypher.openCypher.Skip> skip) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }

  public ProjectionBody withLimit(hydra.util.Maybe<hydra.ext.cypher.openCypher.Limit> limit) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
}
