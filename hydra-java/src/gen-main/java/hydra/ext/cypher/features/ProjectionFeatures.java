// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Projections
 */
public class ProjectionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.ProjectionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_LIMIT = new hydra.core.Name("limit");
  
  public static final hydra.core.Name FIELD_NAME_ORDER_BY = new hydra.core.Name("orderBy");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT_DISTINCT = new hydra.core.Name("projectDistinct");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT_ALL = new hydra.core.Name("projectAll");
  
  public static final hydra.core.Name FIELD_NAME_PROJECT_AS = new hydra.core.Name("projectAs");
  
  public static final hydra.core.Name FIELD_NAME_SKIP = new hydra.core.Name("skip");
  
  public static final hydra.core.Name FIELD_NAME_SORT_ORDER = new hydra.core.Name("sortOrder");
  
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
    java.util.Objects.requireNonNull((limit));
    java.util.Objects.requireNonNull((orderBy));
    java.util.Objects.requireNonNull((projectDistinct));
    java.util.Objects.requireNonNull((projectAll));
    java.util.Objects.requireNonNull((projectAs));
    java.util.Objects.requireNonNull((skip));
    java.util.Objects.requireNonNull((sortOrder));
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
    ProjectionFeatures o = (ProjectionFeatures) (other);
    return limit.equals(o.limit) && orderBy.equals(o.orderBy) && projectDistinct.equals(o.projectDistinct) && projectAll.equals(o.projectAll) && projectAs.equals(o.projectAs) && skip.equals(o.skip) && sortOrder.equals(o.sortOrder);
  }
  
  @Override
  public int hashCode() {
    return 2 * limit.hashCode() + 3 * orderBy.hashCode() + 5 * projectDistinct.hashCode() + 7 * projectAll.hashCode() + 11 * projectAs.hashCode() + 13 * skip.hashCode() + 17 * sortOrder.hashCode();
  }
  
  public ProjectionFeatures withLimit(Boolean limit) {
    java.util.Objects.requireNonNull((limit));
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withOrderBy(Boolean orderBy) {
    java.util.Objects.requireNonNull((orderBy));
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withProjectDistinct(Boolean projectDistinct) {
    java.util.Objects.requireNonNull((projectDistinct));
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withProjectAll(Boolean projectAll) {
    java.util.Objects.requireNonNull((projectAll));
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withProjectAs(Boolean projectAs) {
    java.util.Objects.requireNonNull((projectAs));
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withSkip(Boolean skip) {
    java.util.Objects.requireNonNull((skip));
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
  
  public ProjectionFeatures withSortOrder(Boolean sortOrder) {
    java.util.Objects.requireNonNull((sortOrder));
    return new ProjectionFeatures(limit, orderBy, projectDistinct, projectAll, projectAs, skip, sortOrder);
  }
}