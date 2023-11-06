package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProjectionBody implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProjectionBody");
  
  public final Boolean distinct;
  
  public final hydra.langs.cypher.openCypher.ProjectionItems items;
  
  public final java.util.List<hydra.langs.cypher.openCypher.SortItem> order;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> skip;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> limit;
  
  public ProjectionBody (Boolean distinct, hydra.langs.cypher.openCypher.ProjectionItems items, java.util.List<hydra.langs.cypher.openCypher.SortItem> order, java.util.Optional<hydra.langs.cypher.openCypher.Expression> skip, java.util.Optional<hydra.langs.cypher.openCypher.Expression> limit) {
    this.distinct = distinct;
    this.items = items;
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
    return distinct.equals(o.distinct) && items.equals(o.items) && order.equals(o.order) && skip.equals(o.skip) && limit.equals(o.limit);
  }
  
  @Override
  public int hashCode() {
    return 2 * distinct.hashCode() + 3 * items.hashCode() + 5 * order.hashCode() + 7 * skip.hashCode() + 11 * limit.hashCode();
  }
  
  public ProjectionBody withDistinct(Boolean distinct) {
    return new ProjectionBody(distinct, items, order, skip, limit);
  }
  
  public ProjectionBody withItems(hydra.langs.cypher.openCypher.ProjectionItems items) {
    return new ProjectionBody(distinct, items, order, skip, limit);
  }
  
  public ProjectionBody withOrder(java.util.List<hydra.langs.cypher.openCypher.SortItem> order) {
    return new ProjectionBody(distinct, items, order, skip, limit);
  }
  
  public ProjectionBody withSkip(java.util.Optional<hydra.langs.cypher.openCypher.Expression> skip) {
    return new ProjectionBody(distinct, items, order, skip, limit);
  }
  
  public ProjectionBody withLimit(java.util.Optional<hydra.langs.cypher.openCypher.Expression> limit) {
    return new ProjectionBody(distinct, items, order, skip, limit);
  }
}