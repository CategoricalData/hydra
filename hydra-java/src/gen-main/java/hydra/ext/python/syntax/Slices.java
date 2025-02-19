// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Slices implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Slices");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_TAIL = new hydra.core.Name("tail");
  
  public final hydra.ext.python.syntax.Slice head;
  
  public final java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> tail;
  
  public Slices (hydra.ext.python.syntax.Slice head, java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> tail) {
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((tail));
    this.head = head;
    this.tail = tail;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Slices)) {
      return false;
    }
    Slices o = (Slices) (other);
    return head.equals(o.head) && tail.equals(o.tail);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * tail.hashCode();
  }
  
  public Slices withHead(hydra.ext.python.syntax.Slice head) {
    java.util.Objects.requireNonNull((head));
    return new Slices(head, tail);
  }
  
  public Slices withTail(java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> tail) {
    java.util.Objects.requireNonNull((tail));
    return new Slices(head, tail);
  }
}