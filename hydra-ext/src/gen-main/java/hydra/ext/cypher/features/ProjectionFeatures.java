// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Projections
 */
public class ProjectionFeatures implements Serializable, Comparable<ProjectionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.ProjectionFeatures");
  
  public static final hydra.core.Name LIMIT = new hydra.core.Name("limit");
  
  public static final hydra.core.Name ORDER_BY = new hydra.core.Name("orderBy");
  
  public static final hydra.core.Name PROJECT_DISTINCT = new hydra.core.Name("projectDistinct");
  
  public static final hydra.core.Name PROJECT_ALL = new hydra.core.Name("projectAll");
  
  public static final hydra.core.Name PROJECT_AS = new hydra.core.Name("projectAs");
  
  public static final hydra.core.Name SKIP = new hydra.core.Name("skip");
  
  public static final hydra.core.Name SORT_ORDER = new hydra.core.Name("sortOrder");
  
  /**
   * The LIMIT clause
   */
  public final Boolean limit;
  
  /**
   * The ORDER BY clause
   */
  public final Boolean orderBy;
  
  /**
   * The DISTINCT keyword
   */
  public final Boolean projectDistinct;
  
  /**
   * The * projection
   */
  public final Boolean projectAll;
  
  /**
   * The AS keyword
   */
  public final Boolean projectAs;
  
  /**
   * The SKIP clause
   */
  public final Boolean skip;
  
  /**
   * The ASC/ASCENDING and DESC/DESCENDING keywords
   */
  public final Boolean sortOrder;
  
  public ProjectionFeatures (Boolean limit, Boolean orderBy, Boolean projectDistinct, Boolean projectAll, Boolean projectAs, Boolean skip, Boolean sortOrder) {
    this.limit = limit;
    this.orderBy = orderBy;
    this.projectDistinct = projectDistinct;
    this.projectAll = projectAll;
    this.projectAs = projectAs;
    this.skip = skip;
    this.sortOrder = sortOrder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionFeatures)) {
      return false;
    }
    ProjectionFeatures o = (ProjectionFeatures) other;
    return java.util.Objects.equals(
      this.limit,
      o.limit) && java.util.Objects.equals(
      this.orderBy,
      o.orderBy) && java.util.Objects.equals(
      this.projectDistinct,
      o.projectDistinct) && java.util.Objects.equals(
      this.projectAll,
      o.projectAll) && java.util.Objects.equals(
      this.projectAs,
      o.projectAs) && java.util.Objects.equals(
      this.skip,
      o.skip) && java.util.Objects.equals(
      this.sortOrder,
      o.sortOrder);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(limit) + 3 * java.util.Objects.hashCode(orderBy) + 5 * java.util.Objects.hashCode(projectDistinct) + 7 * java.util.Objects.hashCode(projectAll) + 11 * java.util.Objects.hashCode(projectAs) + 13 * java.util.Objects.hashCode(skip) + 17 * java.util.Objects.hashCode(sortOrder);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProjectionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) limit).compareTo(other.limit);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) orderBy).compareTo(other.orderBy);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) projectDistinct).compareTo(other.projectDistinct);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) projectAll).compareTo(other.projectAll);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) projectAs).compareTo(other.projectAs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) skip).compareTo(other.skip);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) sortOrder).compareTo(other.sortOrder);
  }
  
  public ProjectionFeatures withLimit(Boolean limit) {
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withOrderBy(Boolean orderBy) {
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withProjectDistinct(Boolean projectDistinct) {
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withProjectAll(Boolean projectAll) {
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withProjectAs(Boolean projectAs) {
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withSkip(Boolean skip) {
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withSortOrder(Boolean sortOrder) {
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
}
