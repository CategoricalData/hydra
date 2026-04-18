// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class PathQuery implements Serializable, Comparable<PathQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.PathQuery");

  public static final hydra.core.Name PROJECTION = new hydra.core.Name("projection");

  public static final hydra.core.Name RESTRICTOR_EXT = new hydra.core.Name("restrictorExt");

  public static final hydra.core.Name PATH_PATTERN = new hydra.core.Name("pathPattern");

  public static final hydra.core.Name GROUP_BY = new hydra.core.Name("groupBy");

  public static final hydra.core.Name ORDER_BY = new hydra.core.Name("orderBy");

  public final com.gdblab.pathAlgebra.syntax.Projection projection;

  public final hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.RestrictorExt> restrictorExt;

  public final com.gdblab.pathAlgebra.syntax.PathPattern pathPattern;

  public final hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.GroupBy> groupBy;

  public final hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.OrderBy> orderBy;

  public PathQuery (com.gdblab.pathAlgebra.syntax.Projection projection, hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.RestrictorExt> restrictorExt, com.gdblab.pathAlgebra.syntax.PathPattern pathPattern, hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.GroupBy> groupBy, hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.OrderBy> orderBy) {
    this.projection = projection;
    this.restrictorExt = restrictorExt;
    this.pathPattern = pathPattern;
    this.groupBy = groupBy;
    this.orderBy = orderBy;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathQuery)) {
      return false;
    }
    PathQuery o = (PathQuery) other;
    return java.util.Objects.equals(
      this.projection,
      o.projection) && java.util.Objects.equals(
      this.restrictorExt,
      o.restrictorExt) && java.util.Objects.equals(
      this.pathPattern,
      o.pathPattern) && java.util.Objects.equals(
      this.groupBy,
      o.groupBy) && java.util.Objects.equals(
      this.orderBy,
      o.orderBy);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(projection) + 3 * java.util.Objects.hashCode(restrictorExt) + 5 * java.util.Objects.hashCode(pathPattern) + 7 * java.util.Objects.hashCode(groupBy) + 11 * java.util.Objects.hashCode(orderBy);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PathQuery other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      projection,
      other.projection);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      restrictorExt,
      other.restrictorExt);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      pathPattern,
      other.pathPattern);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      groupBy,
      other.groupBy);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      orderBy,
      other.orderBy);
  }

  public PathQuery withProjection(com.gdblab.pathAlgebra.syntax.Projection projection) {
    return new PathQuery(projection, restrictorExt, pathPattern, groupBy, orderBy);
  }

  public PathQuery withRestrictorExt(hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.RestrictorExt> restrictorExt) {
    return new PathQuery(projection, restrictorExt, pathPattern, groupBy, orderBy);
  }

  public PathQuery withPathPattern(com.gdblab.pathAlgebra.syntax.PathPattern pathPattern) {
    return new PathQuery(projection, restrictorExt, pathPattern, groupBy, orderBy);
  }

  public PathQuery withGroupBy(hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.GroupBy> groupBy) {
    return new PathQuery(projection, restrictorExt, pathPattern, groupBy, orderBy);
  }

  public PathQuery withOrderBy(hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.OrderBy> orderBy) {
    return new PathQuery(projection, restrictorExt, pathPattern, groupBy, orderBy);
  }
}
