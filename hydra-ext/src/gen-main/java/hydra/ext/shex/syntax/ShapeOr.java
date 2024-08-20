// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class ShapeOr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeOr");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_AND = new hydra.core.Name("shapeAnd");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.ext.shex.syntax.ShapeAnd shapeAnd;
  
  public final java.util.List<hydra.ext.shex.syntax.ShapeAnd> listOfSequence;
  
  public ShapeOr (hydra.ext.shex.syntax.ShapeAnd shapeAnd, java.util.List<hydra.ext.shex.syntax.ShapeAnd> listOfSequence) {
    java.util.Objects.requireNonNull((shapeAnd));
    java.util.Objects.requireNonNull((listOfSequence));
    this.shapeAnd = shapeAnd;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeOr)) {
      return false;
    }
    ShapeOr o = (ShapeOr) (other);
    return shapeAnd.equals(o.shapeAnd) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeAnd.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public ShapeOr withShapeAnd(hydra.ext.shex.syntax.ShapeAnd shapeAnd) {
    java.util.Objects.requireNonNull((shapeAnd));
    return new ShapeOr(shapeAnd, listOfSequence);
  }
  
  public ShapeOr withListOfSequence(java.util.List<hydra.ext.shex.syntax.ShapeAnd> listOfSequence) {
    java.util.Objects.requireNonNull((listOfSequence));
    return new ShapeOr(shapeAnd, listOfSequence);
  }
}