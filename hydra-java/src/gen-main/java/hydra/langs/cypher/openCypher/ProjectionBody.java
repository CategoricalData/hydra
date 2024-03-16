package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProjectionBody implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProjectionBody");
  
  public final Boolean distinct;
  
  public final hydra.langs.cypher.openCypher.ProjectionItems projectionItems;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Order> order;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Skip> skip;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Limit> limit;
  
  public ProjectionBody (Boolean distinct, hydra.langs.cypher.openCypher.ProjectionItems projectionItems, java.util.Optional<hydra.langs.cypher.openCypher.Order> order, java.util.Optional<hydra.langs.cypher.openCypher.Skip> skip, java.util.Optional<hydra.langs.cypher.openCypher.Limit> limit) {
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
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withProjectionItems(hydra.langs.cypher.openCypher.ProjectionItems projectionItems) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withOrder(java.util.Optional<hydra.langs.cypher.openCypher.Order> order) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withSkip(java.util.Optional<hydra.langs.cypher.openCypher.Skip> skip) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withLimit(java.util.Optional<hydra.langs.cypher.openCypher.Limit> limit) {
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
}