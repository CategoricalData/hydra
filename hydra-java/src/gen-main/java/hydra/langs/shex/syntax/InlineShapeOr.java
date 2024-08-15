// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class InlineShapeOr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.InlineShapeOr");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_AND = new hydra.core.Name("shapeAnd");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.langs.shex.syntax.ShapeAnd shapeAnd;
  
  public final java.util.List<hydra.langs.shex.syntax.InlineShapeAnd> listOfSequence;
  
  public InlineShapeOr (hydra.langs.shex.syntax.ShapeAnd shapeAnd, java.util.List<hydra.langs.shex.syntax.InlineShapeAnd> listOfSequence) {
    java.util.Objects.requireNonNull((shapeAnd));
    java.util.Objects.requireNonNull((listOfSequence));
    this.shapeAnd = shapeAnd;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeOr)) {
      return false;
    }
    InlineShapeOr o = (InlineShapeOr) (other);
    return shapeAnd.equals(o.shapeAnd) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeAnd.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public InlineShapeOr withShapeAnd(hydra.langs.shex.syntax.ShapeAnd shapeAnd) {
    java.util.Objects.requireNonNull((shapeAnd));
    return new InlineShapeOr(shapeAnd, listOfSequence);
  }
  
  public InlineShapeOr withListOfSequence(java.util.List<hydra.langs.shex.syntax.InlineShapeAnd> listOfSequence) {
    java.util.Objects.requireNonNull((listOfSequence));
    return new InlineShapeOr(shapeAnd, listOfSequence);
  }
}