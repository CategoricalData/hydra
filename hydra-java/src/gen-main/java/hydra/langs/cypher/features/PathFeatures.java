// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for path functions.
 */
public class PathFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.PathFeatures");
  
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
    if (length == null) {
      throw new IllegalArgumentException("null value for 'length' argument");
    }
    if (nodes == null) {
      throw new IllegalArgumentException("null value for 'nodes' argument");
    }
    if (relationships == null) {
      throw new IllegalArgumentException("null value for 'relationships' argument");
    }
    if (shortestPath == null) {
      throw new IllegalArgumentException("null value for 'shortestPath' argument");
    }
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
    if (length == null) {
      throw new IllegalArgumentException("null value for 'length' argument");
    }
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
  
  public PathFeatures withNodes(Boolean nodes) {
    if (nodes == null) {
      throw new IllegalArgumentException("null value for 'nodes' argument");
    }
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
  
  public PathFeatures withRelationships(Boolean relationships) {
    if (relationships == null) {
      throw new IllegalArgumentException("null value for 'relationships' argument");
    }
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
  
  public PathFeatures withShortestPath(Boolean shortestPath) {
    if (shortestPath == null) {
      throw new IllegalArgumentException("null value for 'shortestPath' argument");
    }
    return new PathFeatures(length, nodes, relationships, shortestPath);
  }
}