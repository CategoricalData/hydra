package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProjectionItems implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProjectionItems");
  
  public final Boolean all;
  
  public final java.util.List<hydra.langs.cypher.openCypher.ProjectionItem> items;
  
  public ProjectionItems (Boolean all, java.util.List<hydra.langs.cypher.openCypher.ProjectionItem> items) {
    this.all = all;
    this.items = items;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionItems)) {
      return false;
    }
    ProjectionItems o = (ProjectionItems) (other);
    return all.equals(o.all) && items.equals(o.items);
  }
  
  @Override
  public int hashCode() {
    return 2 * all.hashCode() + 3 * items.hashCode();
  }
  
  public ProjectionItems withAll(Boolean all) {
    return new ProjectionItems(all, items);
  }
  
  public ProjectionItems withItems(java.util.List<hydra.langs.cypher.openCypher.ProjectionItem> items) {
    return new ProjectionItems(all, items);
  }
}