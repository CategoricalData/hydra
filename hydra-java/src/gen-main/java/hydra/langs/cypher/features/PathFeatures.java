// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Path functions only found in OpenCypher
 */
public class PathFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.PathFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SHORTEST_PATH = new hydra.core.Name("shortestPath");
  
  /**
   * The shortestPath() function
   */
  public final Boolean shortestPath;
  
  public PathFeatures (Boolean shortestPath) {
    java.util.Objects.requireNonNull((shortestPath));
    this.shortestPath = shortestPath;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathFeatures)) {
      return false;
    }
    PathFeatures o = (PathFeatures) (other);
    return shortestPath.equals(o.shortestPath);
  }
  
  @Override
  public int hashCode() {
    return 2 * shortestPath.hashCode();
  }
}