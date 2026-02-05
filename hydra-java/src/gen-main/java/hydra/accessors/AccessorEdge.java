// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * An edge in an accessor graph, connecting two nodes via a path
 */
public class AccessorEdge implements Serializable, Comparable<AccessorEdge> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.accessors.AccessorEdge");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_PATH = new hydra.core.Name("path");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  /**
   * The source node of the edge
   */
  public final hydra.accessors.AccessorNode source;
  
  /**
   * The accessor path connecting source to target
   */
  public final hydra.accessors.AccessorPath path;
  
  /**
   * The target node of the edge
   */
  public final hydra.accessors.AccessorNode target;
  
  public AccessorEdge (hydra.accessors.AccessorNode source, hydra.accessors.AccessorPath path, hydra.accessors.AccessorNode target) {
    this.source = source;
    this.path = path;
    this.target = target;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AccessorEdge)) {
      return false;
    }
    AccessorEdge o = (AccessorEdge) (other);
    return java.util.Objects.equals(
      this.source,
      o.source) && java.util.Objects.equals(
      this.path,
      o.path) && java.util.Objects.equals(
      this.target,
      o.target);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(source) + 3 * java.util.Objects.hashCode(path) + 5 * java.util.Objects.hashCode(target);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AccessorEdge other) {
    int cmp = 0;
    cmp = ((Comparable) (source)).compareTo(other.source);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (path)).compareTo(other.path);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (target)).compareTo(other.target);
  }
  
  public AccessorEdge withSource(hydra.accessors.AccessorNode source) {
    return new AccessorEdge(source, path, target);
  }
  
  public AccessorEdge withPath(hydra.accessors.AccessorPath path) {
    return new AccessorEdge(source, path, target);
  }
  
  public AccessorEdge withTarget(hydra.accessors.AccessorNode target) {
    return new AccessorEdge(source, path, target);
  }
}
