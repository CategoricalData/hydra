// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for path functions.
 */
public class PathFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.PathFeatures");
  
  public static final hydra.core.Name FIELD_NAME_LENGTH = new hydra.core.Name("length");
  
  public static final hydra.core.Name FIELD_NAME_NODES = new hydra.core.Name("nodes");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIPS = new hydra.core.Name("relationships");
  
  public static final hydra.core.Name FIELD_NAME_SHORTEST_PATH = new hydra.core.Name("shortestPath");
  
  /**
   * Whether to expect the length() function.
   */
  public final Boolean length;
  
  /**
   * Whether to expect the nodes() function.
   */
  public final Boolean nodes;
  
  /**
   * Whether to expect the relationships() function.
   */
  public final Boolean relationships;
  
  /**
   * Whether to expect the shortestPath() function.
   */
  public final Boolean shortestPath;
  
  public PathFeatures (Boolean length, Boolean nodes, Boolean relationships, Boolean shortestPath) {
    java.util.Objects.requireNonNull((length));
    java.util.Objects.requireNonNull((nodes));
    java.util.Objects.requireNonNull((relationships));
    java.util.Objects.requireNonNull((shortestPath));
    this.length = length;
    this.nodes = nodes;
    this.relationships = relationships;
    this.shortestPath = shortestPath;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathFeatures)) {
      return false;
    }
    PathFeatures o = (PathFeatures) (other);
    return length.equals(o.length) && nodes.equals(o.nodes) && relationships.equals(o.relationships) && shortestPath.equals(o.shortestPath);
  }
  
  @Override
  public int hashCode() {
    return 2 * length.hashCode() + 3 * nodes.hashCode() + 5 * relationships.hashCode() + 7 * shortestPath.hashCode();
  }
  
  public PathFeatures withLength(Boolean length) {
    java.util.Objects.requireNonNull((length));
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
  
  public PathFeatures withNodes(Boolean nodes) {
    java.util.Objects.requireNonNull((nodes));
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
  
  public PathFeatures withRelationships(Boolean relationships) {
    java.util.Objects.requireNonNull((relationships));
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
  
  public PathFeatures withShortestPath(Boolean shortestPath) {
    java.util.Objects.requireNonNull((shortestPath));
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
}