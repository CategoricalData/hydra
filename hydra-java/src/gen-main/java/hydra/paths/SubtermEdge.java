// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * An edge in a subterm graph, connecting two nodes via a path
 */
public class SubtermEdge implements Serializable, Comparable<SubtermEdge> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtermEdge");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  /**
   * The source node of the edge
   */
  public final hydra.paths.SubtermNode source;

  /**
   * The subterm path connecting source to target
   */
  public final hydra.paths.SubtermPath path;

  /**
   * The target node of the edge
   */
  public final hydra.paths.SubtermNode target;

  public SubtermEdge (hydra.paths.SubtermNode source, hydra.paths.SubtermPath path, hydra.paths.SubtermNode target) {
    this.source = source;
    this.path = path;
    this.target = target;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtermEdge)) {
      return false;
    }
    SubtermEdge o = (SubtermEdge) other;
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
  public int compareTo(SubtermEdge other) {
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

  public SubtermEdge withSource(hydra.paths.SubtermNode source) {
    return new SubtermEdge(source, path, target);
  }

  public SubtermEdge withPath(hydra.paths.SubtermPath path) {
    return new SubtermEdge(source, path, target);
  }

  public SubtermEdge withTarget(hydra.paths.SubtermNode target) {
    return new SubtermEdge(source, path, target);
  }
}
