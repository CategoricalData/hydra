// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * Path functions only found in OpenCypher
 */
public class PathFeatures implements Serializable, Comparable<PathFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.PathFeatures");

  public static final hydra.core.Name SHORTEST_PATH = new hydra.core.Name("shortestPath");

  /**
   * The shortestPath() function
   */
  public final Boolean shortestPath;

  public PathFeatures (Boolean shortestPath) {
    this.shortestPath = shortestPath;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathFeatures)) {
      return false;
    }
    PathFeatures o = (PathFeatures) other;
    return java.util.Objects.equals(
      this.shortestPath,
      o.shortestPath);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(shortestPath);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PathFeatures other) {
    return hydra.util.Comparing.compare(
      shortestPath,
      other.shortestPath);
  }
}
