// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class ShapeAnd implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.ShapeAnd");
  
  public final hydra.langs.shex.syntax.ShapeNot shapeNot;
  
  public final java.util.List<hydra.langs.shex.syntax.ShapeNot> listOfSequence;
  
  public ShapeAnd (hydra.langs.shex.syntax.ShapeNot shapeNot, java.util.List<hydra.langs.shex.syntax.ShapeNot> listOfSequence) {
    if (shapeNot == null) {
      throw new IllegalArgumentException("null value for 'shapeNot' argument");
    }
    if (listOfSequence == null) {
      throw new IllegalArgumentException("null value for 'listOfSequence' argument");
    }
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
  
  public ShapeAnd withShapeNot(hydra.langs.shex.syntax.ShapeNot shapeNot) {
    if (shapeNot == null) {
      throw new IllegalArgumentException("null value for 'shapeNot' argument");
    }
    return new ShapeAnd(shapeNot, listOfSequence);
  }
  
  public ShapeAnd withListOfSequence(java.util.List<hydra.langs.shex.syntax.ShapeNot> listOfSequence) {
    if (listOfSequence == null) {
      throw new IllegalArgumentException("null value for 'listOfSequence' argument");
    }
    return new ShapeAnd(shapeNot, listOfSequence);
  }
}