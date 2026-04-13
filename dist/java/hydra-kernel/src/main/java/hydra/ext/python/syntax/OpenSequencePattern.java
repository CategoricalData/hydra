// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class OpenSequencePattern implements Serializable, Comparable<OpenSequencePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.OpenSequencePattern");

  public static final hydra.core.Name HEAD = new hydra.core.Name("head");

  public static final hydra.core.Name TAIL = new hydra.core.Name("tail");

  public final hydra.ext.python.syntax.MaybeStarPattern head;

  public final hydra.util.Maybe<hydra.ext.python.syntax.MaybeSequencePattern> tail;

  public OpenSequencePattern (hydra.ext.python.syntax.MaybeStarPattern head, hydra.util.Maybe<hydra.ext.python.syntax.MaybeSequencePattern> tail) {
    this.head = head;
    this.tail = tail;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpenSequencePattern)) {
      return false;
    }
    OpenSequencePattern o = (OpenSequencePattern) other;
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
  public int compareTo(OpenSequencePattern other) {
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

  public OpenSequencePattern withHead(hydra.ext.python.syntax.MaybeStarPattern head) {
    return new OpenSequencePattern(head, tail);
  }

  public OpenSequencePattern withTail(hydra.util.Maybe<hydra.ext.python.syntax.MaybeSequencePattern> tail) {
    return new OpenSequencePattern(head, tail);
  }
}
