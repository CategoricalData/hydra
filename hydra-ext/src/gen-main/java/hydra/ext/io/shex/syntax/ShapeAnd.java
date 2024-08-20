// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShapeAnd implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.ShapeAnd");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_NOT = new hydra.core.Name("shapeNot");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.ext.io.shex.syntax.ShapeNot shapeNot;
  
  public final java.util.List<hydra.ext.io.shex.syntax.ShapeNot> listOfSequence;
  
  public ShapeAnd (hydra.ext.io.shex.syntax.ShapeNot shapeNot, java.util.List<hydra.ext.io.shex.syntax.ShapeNot> listOfSequence) {
    java.util.Objects.requireNonNull((shapeNot));
    java.util.Objects.requireNonNull((listOfSequence));
    this.shapeNot = shapeNot;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeAnd)) {
      return false;
    }
    ShapeAnd o = (ShapeAnd) (other);
    return shapeNot.equals(o.shapeNot) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeNot.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public ShapeAnd withShapeNot(hydra.ext.io.shex.syntax.ShapeNot shapeNot) {
    java.util.Objects.requireNonNull((shapeNot));
    return new ShapeAnd(shapeNot, listOfSequence);
  }
  
  public ShapeAnd withListOfSequence(java.util.List<hydra.ext.io.shex.syntax.ShapeNot> listOfSequence) {
    java.util.Objects.requireNonNull((listOfSequence));
    return new ShapeAnd(shapeNot, listOfSequence);
  }
}