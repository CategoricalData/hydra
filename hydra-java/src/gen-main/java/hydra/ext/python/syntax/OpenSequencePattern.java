// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class OpenSequencePattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.OpenSequencePattern");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_TAIL = new hydra.core.Name("tail");
  
  public final hydra.ext.python.syntax.MaybeStarPattern head;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.MaybeSequencePattern> tail;
  
  public OpenSequencePattern (hydra.ext.python.syntax.MaybeStarPattern head, hydra.util.Opt<hydra.ext.python.syntax.MaybeSequencePattern> tail) {
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((tail));
    this.head = head;
    this.tail = tail;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpenSequencePattern)) {
      return false;
    }
    OpenSequencePattern o = (OpenSequencePattern) (other);
    return head.equals(o.head) && tail.equals(o.tail);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * tail.hashCode();
  }
  
  public OpenSequencePattern withHead(hydra.ext.python.syntax.MaybeStarPattern head) {
    java.util.Objects.requireNonNull((head));
    return new OpenSequencePattern(head, tail);
  }
  
  public OpenSequencePattern withTail(hydra.util.Opt<hydra.ext.python.syntax.MaybeSequencePattern> tail) {
    java.util.Objects.requireNonNull((tail));
    return new OpenSequencePattern(head, tail);
  }
}