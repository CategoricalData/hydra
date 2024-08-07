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
    java.util.Objects.requireNonNull((distinct));
    java.util.Objects.requireNonNull((projectionItems));
    java.util.Objects.requireNonNull((order));
    java.util.Objects.requireNonNull((skip));
    java.util.Objects.requireNonNull((limit));
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
    java.util.Objects.requireNonNull((distinct));
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withProjectionItems(hydra.langs.cypher.openCypher.ProjectionItems projectionItems) {
    java.util.Objects.requireNonNull((projectionItems));
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withOrder(hydra.util.Opt<hydra.langs.cypher.openCypher.Order> order) {
    java.util.Objects.requireNonNull((order));
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withSkip(hydra.util.Opt<hydra.langs.cypher.openCypher.Skip> skip) {
    java.util.Objects.requireNonNull((skip));
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
  
  public ProjectionBody withLimit(hydra.util.Opt<hydra.langs.cypher.openCypher.Limit> limit) {
    java.util.Objects.requireNonNull((limit));
    return new ProjectionBody(distinct, projectionItems, order, skip, limit);
  }
}