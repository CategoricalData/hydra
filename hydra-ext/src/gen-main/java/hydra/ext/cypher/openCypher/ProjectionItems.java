// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ProjectionItems implements Serializable, Comparable<ProjectionItems> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ProjectionItems");
  
  public static final hydra.core.Name STAR = new hydra.core.Name("star");
  
  public static final hydra.core.Name EXPLICIT = new hydra.core.Name("explicit");
  
  public final Boolean star;
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.ProjectionItem> explicit;
  
  public ProjectionItems (Boolean star, hydra.util.ConsList<hydra.ext.cypher.openCypher.ProjectionItem> explicit) {
    this.star = star;
    this.explicit = explicit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionItems)) {
      return false;
    }
    ProjectionItems o = (ProjectionItems) other;
    return java.util.Objects.equals(
      this.star,
      o.star) && java.util.Objects.equals(
      this.explicit,
      o.explicit);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(star) + 3 * java.util.Objects.hashCode(explicit);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProjectionItems other) {
    int cmp = 0;
    cmp = ((Comparable) star).compareTo(other.star);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      explicit.hashCode(),
      other.explicit.hashCode());
  }
  
  public ProjectionItems withStar(Boolean star) {
    return new ProjectionItems(star, explicit);
  }
  
  public ProjectionItems withExplicit(hydra.util.ConsList<hydra.ext.cypher.openCypher.ProjectionItem> explicit) {
    return new ProjectionItems(star, explicit);
  }
}
