// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProjectionBody implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProjectionBody");
  
  public final Boolean distinct;
  
  public final hydra.langs.cypher.openCypher.ProjectionItems projectionItems;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Order> order;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Skip> skip;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Limit> limit;
  
  public ProjectionBody (Boolean distinct, hydra.langs.cypher.openCypher.ProjectionItems projectionItems, hydra.util.Opt<hydra.langs.cypher.openCypher.Order> order, hydra.util.Opt<hydra.langs.cypher.openCypher.Skip> skip, hydra.util.Opt<hydra.langs.cypher.openCypher.Limit> limit) {
    if (distinct == null) {
      throw new IllegalArgumentException("null value for 'distinct' argument");
    }
    if (projectionItems == null) {
      throw new IllegalArgumentException("null value for 'projectionItems' argument");
    }
    if (order == null) {
      throw new IllegalArgumentException("null value for 'order' argument");
    }
    if (skip == null) {
      throw new IllegalArgumentException("null value for 'skip' argument");
    }
    if (limit == null) {
      throw new IllegalArgumentException("null value for 'limit' argument");
    }
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
    ProjectionBody o = (ProjectionBody) (other);
    return distinct.equals(o.distinct) && projectionItems.equals(o.projectionItems) && order.equals(o.order) && skip.equals(o.skip) && limit.equals(o.limit);
  }
  
  @Override
  public int hashCode() {
    return 2 * distinct.hashCode() + 3 * projectionItems.hashCode() + 5 * order.hashCode() + 7 * skip.hashCode() + 11 * limit.hashCode();
  }
  
  public ProjectionBody withDistinct(Boolean distinct) {
    if (distinct == null) {
      throw new IllegalArgumentException("null value for 'distinct' argument");
    }
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withProjectionItems(hydra.langs.cypher.openCypher.ProjectionItems projectionItems) {
    if (projectionItems == null) {
      throw new IllegalArgumentException("null value for 'projectionItems' argument");
    }
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withOrder(hydra.util.Opt<hydra.langs.cypher.openCypher.Order> order) {
    if (order == null) {
      throw new IllegalArgumentException("null value for 'order' argument");
    }
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withSkip(hydra.util.Opt<hydra.langs.cypher.openCypher.Skip> skip) {
    if (skip == null) {
      throw new IllegalArgumentException("null value for 'skip' argument");
    }
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withLimit(hydra.util.Opt<hydra.langs.cypher.openCypher.Limit> limit) {
    if (limit == null) {
      throw new IllegalArgumentException("null value for 'limit' argument");
    }
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
}