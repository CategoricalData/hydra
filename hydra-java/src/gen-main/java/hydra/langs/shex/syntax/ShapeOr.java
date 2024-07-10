// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class ShapeOr implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.ShapeOr");
  
  public final hydra.langs.shex.syntax.ShapeAnd shapeAnd;
  
  public final java.util.List<hydra.langs.shex.syntax.ShapeAnd> listOfSequence;
  
  public ShapeOr (hydra.langs.shex.syntax.ShapeAnd shapeAnd, java.util.List<hydra.langs.shex.syntax.ShapeAnd> listOfSequence) {
    if (shapeAnd == null) {
      throw new IllegalArgumentException("null value for 'shapeAnd' argument");
    }
    if (listOfSequence == null) {
      throw new IllegalArgumentException("null value for 'listOfSequence' argument");
    }
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
  
  public ShapeOr withShapeAnd(hydra.langs.shex.syntax.ShapeAnd shapeAnd) {
    if (shapeAnd == null) {
      throw new IllegalArgumentException("null value for 'shapeAnd' argument");
    }
    return new ShapeOr(shapeAnd, listOfSequence);
  }
  
  public ShapeOr withListOfSequence(java.util.List<hydra.langs.shex.syntax.ShapeAnd> listOfSequence) {
    if (listOfSequence == null) {
      throw new IllegalArgumentException("null value for 'listOfSequence' argument");
    }
    return new ShapeOr(shapeAnd, listOfSequence);
  }
}