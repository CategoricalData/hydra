// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A cons expression: (cons head tail)
 */
public class ConsExpression implements Serializable, Comparable<ConsExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.ConsExpression");

  public static final hydra.core.Name HEAD = new hydra.core.Name("head");

  public static final hydra.core.Name TAIL = new hydra.core.Name("tail");

  /**
   * The head element
   */
  public final hydra.ext.lisp.syntax.Expression head;

  /**
   * The tail (typically a list or another cons)
   */
  public final hydra.ext.lisp.syntax.Expression tail;

  public ConsExpression (hydra.ext.lisp.syntax.Expression head, hydra.ext.lisp.syntax.Expression tail) {
    this.head = head;
    this.tail = tail;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConsExpression)) {
      return false;
    }
    ConsExpression o = (ConsExpression) other;
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
  public int compareTo(ConsExpression other) {
    int cmp = 0;
    cmp = ((Comparable) head).compareTo(other.head);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) tail).compareTo(other.tail);
  }

  public ConsExpression withHead(hydra.ext.lisp.syntax.Expression head) {
    return new ConsExpression(head, tail);
  }

  public ConsExpression withTail(hydra.ext.lisp.syntax.Expression tail) {
    return new ConsExpression(head, tail);
  }
}
