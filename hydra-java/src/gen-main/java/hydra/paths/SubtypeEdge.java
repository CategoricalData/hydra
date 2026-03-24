// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * An edge in a subtype graph, connecting two nodes via a path
 */
public class SubtypeEdge implements Serializable, Comparable<SubtypeEdge> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtypeEdge");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  /**
   * The source node of the edge
   */
  public final hydra.paths.SubtypeNode source;

  /**
   * The subtype path connecting source to target
   */
  public final hydra.paths.SubtypePath path;

  /**
   * The target node of the edge
   */
  public final hydra.paths.SubtypeNode target;

  public SubtypeEdge (hydra.paths.SubtypeNode source, hydra.paths.SubtypePath path, hydra.paths.SubtypeNode target) {
    this.source = source;
    this.path = path;
    this.target = target;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtypeEdge)) {
      return false;
    }
    SubtypeEdge o = (SubtypeEdge) other;
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
  public int compareTo(SubtypeEdge other) {
    int cmp = 0;
    cmp = ((Comparable) source).compareTo(other.source);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) path).compareTo(other.path);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) target).compareTo(other.target);
  }

  public SubtypeEdge withSource(hydra.paths.SubtypeNode source) {
    return new SubtypeEdge(source, path, target);
  }

  public SubtypeEdge withPath(hydra.paths.SubtypePath path) {
    return new SubtypeEdge(source, path, target);
  }

  public SubtypeEdge withTarget(hydra.paths.SubtypeNode target) {
    return new SubtypeEdge(source, path, target);
  }
}
