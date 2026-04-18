// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Slices implements Serializable, Comparable<Slices> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Slices");

  public static final hydra.core.Name HEAD = new hydra.core.Name("head");

  public static final hydra.core.Name TAIL = new hydra.core.Name("tail");

  public final hydra.python.syntax.Slice head;

  public final java.util.List<hydra.python.syntax.SliceOrStarredExpression> tail;

  public Slices (hydra.python.syntax.Slice head, java.util.List<hydra.python.syntax.SliceOrStarredExpression> tail) {
    this.head = head;
    this.tail = tail;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Slices)) {
      return false;
    }
    Slices o = (Slices) other;
    return java.util.Objects.equals(
      this.head,
      o.head) && java.util.Objects.equals(
      this.tail,
      o.tail);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(head) + 3 * java.util.Objects.hashCode(tail);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Slices other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      head,
      other.head);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tail,
      other.tail);
  }

  public Slices withHead(hydra.python.syntax.Slice head) {
    return new Slices(head, tail);
  }

  public Slices withTail(java.util.List<hydra.python.syntax.SliceOrStarredExpression> tail) {
    return new Slices(head, tail);
  }
}
