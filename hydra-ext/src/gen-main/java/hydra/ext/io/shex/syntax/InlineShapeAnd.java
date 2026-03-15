// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class InlineShapeAnd implements Serializable, Comparable<InlineShapeAnd> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAnd");
  
  public static final hydra.core.Name INLINE_SHAPE_NOT = new hydra.core.Name("InlineShapeNot");
  
  public static final hydra.core.Name LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.ext.io.shex.syntax.InlineShapeNot InlineShapeNot;
  
  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.InlineShapeNot> listOfSequence;
  
  public InlineShapeAnd (hydra.ext.io.shex.syntax.InlineShapeNot InlineShapeNot, hydra.util.ConsList<hydra.ext.io.shex.syntax.InlineShapeNot> listOfSequence) {
    this.InlineShapeNot = InlineShapeNot;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAnd)) {
      return false;
    }
    InlineShapeAnd o = (InlineShapeAnd) other;
    return java.util.Objects.equals(
      this.InlineShapeNot,
      o.InlineShapeNot) && java.util.Objects.equals(
      this.listOfSequence,
      o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(InlineShapeNot) + 3 * java.util.Objects.hashCode(listOfSequence);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InlineShapeAnd other) {
    int cmp = 0;
    cmp = ((Comparable) InlineShapeNot).compareTo(other.InlineShapeNot);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      listOfSequence.hashCode(),
      other.listOfSequence.hashCode());
  }
  
  public InlineShapeAnd withInlineShapeNot(hydra.ext.io.shex.syntax.InlineShapeNot InlineShapeNot) {
    return new InlineShapeAnd(InlineShapeNot, listOfSequence);
  }
  
  public InlineShapeAnd withListOfSequence(hydra.util.ConsList<hydra.ext.io.shex.syntax.InlineShapeNot> listOfSequence) {
    return new InlineShapeAnd(InlineShapeNot, listOfSequence);
  }
}
