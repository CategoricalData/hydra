// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Genexp implements Serializable, Comparable<Genexp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Genexp");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_TAIL = new hydra.core.Name("tail");
  
  public final hydra.ext.python.syntax.GenexpHead head;
  
  public final hydra.ext.python.syntax.ForIfClauses tail;
  
  public Genexp (hydra.ext.python.syntax.GenexpHead head, hydra.ext.python.syntax.ForIfClauses tail) {
    this.head = head;
    this.tail = tail;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Genexp)) {
      return false;
    }
    Genexp o = (Genexp) other;
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
  public int compareTo(Genexp other) {
    int cmp = 0;
    cmp = ((Comparable) head).compareTo(other.head);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) tail).compareTo(other.tail);
  }
  
  public Genexp withHead(hydra.ext.python.syntax.GenexpHead head) {
    return new Genexp(head, tail);
  }
  
  public Genexp withTail(hydra.ext.python.syntax.ForIfClauses tail) {
    return new Genexp(head, tail);
  }
}
