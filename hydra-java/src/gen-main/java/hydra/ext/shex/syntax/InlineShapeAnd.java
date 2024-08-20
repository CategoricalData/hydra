// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class InlineShapeAnd implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeAnd");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_SHAPE_NOT = new hydra.core.Name("inlineShapeNot");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot;
  
  public final java.util.List<hydra.ext.shex.syntax.InlineShapeNot> listOfSequence;
  
  public InlineShapeAnd (hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot, java.util.List<hydra.ext.shex.syntax.InlineShapeNot> listOfSequence) {
    java.util.Objects.requireNonNull((inlineShapeNot));
    java.util.Objects.requireNonNull((listOfSequence));
    this.inlineShapeNot = inlineShapeNot;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAnd)) {
      return false;
    }
    InlineShapeAnd o = (InlineShapeAnd) (other);
    return inlineShapeNot.equals(o.inlineShapeNot) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * inlineShapeNot.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public InlineShapeAnd withInlineShapeNot(hydra.ext.shex.syntax.InlineShapeNot inlineShapeNot) {
    java.util.Objects.requireNonNull((inlineShapeNot));
    return new InlineShapeAnd(inlineShapeNot, listOfSequence);
  }
  
  public InlineShapeAnd withListOfSequence(java.util.List<hydra.ext.shex.syntax.InlineShapeNot> listOfSequence) {
    java.util.Objects.requireNonNull((listOfSequence));
    return new InlineShapeAnd(inlineShapeNot, listOfSequence);
  }
}
