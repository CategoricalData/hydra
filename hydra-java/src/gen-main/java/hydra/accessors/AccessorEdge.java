// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * An edge in an accessor graph, connecting two nodes via a path
 */
public class AccessorEdge implements Serializable {
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
    java.util.Objects.requireNonNull((source));
    java.util.Objects.requireNonNull((path));
    java.util.Objects.requireNonNull((target));
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
    return source.equals(o.source) && path.equals(o.path) && target.equals(o.target);
  }
  
  @Override
  public int hashCode() {
    return 2 * source.hashCode() + 3 * path.hashCode() + 5 * target.hashCode();
  }
  
  public AccessorEdge withSource(hydra.accessors.AccessorNode source) {
    java.util.Objects.requireNonNull((source));
    return new AccessorEdge(source, path, target);
  }
  
  public AccessorEdge withPath(hydra.accessors.AccessorPath path) {
    java.util.Objects.requireNonNull((path));
    return new AccessorEdge(source, path, target);
  }
  
  public AccessorEdge withTarget(hydra.accessors.AccessorNode target) {
    java.util.Objects.requireNonNull((target));
    return new AccessorEdge(source, path, target);
  }
}
