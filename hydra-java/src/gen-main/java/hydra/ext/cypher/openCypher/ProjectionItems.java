// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ProjectionItems implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.ProjectionItems");
  
  public static final hydra.core.Name FIELD_NAME_STAR = new hydra.core.Name("star");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICIT = new hydra.core.Name("explicit");
  
  public final Boolean star;
  
  public final java.util.List<hydra.ext.cypher.openCypher.ProjectionItem> explicit;
  
  public ProjectionItems (Boolean star, java.util.List<hydra.ext.cypher.openCypher.ProjectionItem> explicit) {
    java.util.Objects.requireNonNull((star));
    java.util.Objects.requireNonNull((explicit));
    this.star = star;
    this.explicit = explicit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionItems)) {
      return false;
    }
    ProjectionItems o = (ProjectionItems) (other);
    return star.equals(o.star) && explicit.equals(o.explicit);
  }
  
  @Override
  public int hashCode() {
    return 2 * star.hashCode() + 3 * explicit.hashCode();
  }
  
  public ProjectionItems withStar(Boolean star) {
    java.util.Objects.requireNonNull((star));
    return new ProjectionItems(star, explicit);
  }
  
  public ProjectionItems withExplicit(java.util.List<hydra.ext.cypher.openCypher.ProjectionItem> explicit) {
    java.util.Objects.requireNonNull((explicit));
    return new ProjectionItems(star, explicit);
  }
}