// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Genexp implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Genexp");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_TAIL = new hydra.core.Name("tail");
  
  public final hydra.ext.python.syntax.GenexpHead head;
  
  public final hydra.ext.python.syntax.ForIfClauses tail;
  
  public Genexp (hydra.ext.python.syntax.GenexpHead head, hydra.ext.python.syntax.ForIfClauses tail) {
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((tail));
    this.head = head;
    this.tail = tail;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Genexp)) {
      return false;
    }
    Genexp o = (Genexp) (other);
    return head.equals(o.head) && tail.equals(o.tail);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * tail.hashCode();
  }
  
  public Genexp withHead(hydra.ext.python.syntax.GenexpHead head) {
    java.util.Objects.requireNonNull((head));
    return new Genexp(head, tail);
  }
  
  public Genexp withTail(hydra.ext.python.syntax.ForIfClauses tail) {
    java.util.Objects.requireNonNull((tail));
    return new Genexp(head, tail);
  }
}