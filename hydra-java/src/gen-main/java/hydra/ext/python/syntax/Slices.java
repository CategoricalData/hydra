// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Slices implements Serializable, Comparable<Slices> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Slices");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_TAIL = new hydra.core.Name("tail");
  
  public final hydra.ext.python.syntax.Slice head;
  
  public final java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> tail;
  
  public Slices (hydra.ext.python.syntax.Slice head, java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> tail) {
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
    cmp = ((Comparable) head).compareTo(other.head);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      tail.hashCode(),
      other.tail.hashCode());
  }
  
  public Slices withHead(hydra.ext.python.syntax.Slice head) {
    return new Slices(head, tail);
  }
  
  public Slices withTail(java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> tail) {
    return new Slices(head, tail);
  }
}
