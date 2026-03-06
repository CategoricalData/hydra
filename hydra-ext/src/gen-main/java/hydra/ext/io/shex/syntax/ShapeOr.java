// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShapeOr implements Serializable, Comparable<ShapeOr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeOr");
  
  public static final hydra.core.Name SHAPE_AND = new hydra.core.Name("ShapeAnd");
  
  public static final hydra.core.Name LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.ext.io.shex.syntax.ShapeAnd ShapeAnd;
  
  public final java.util.List<hydra.ext.io.shex.syntax.ShapeAnd> listOfSequence;
  
  public ShapeOr (hydra.ext.io.shex.syntax.ShapeAnd ShapeAnd, java.util.List<hydra.ext.io.shex.syntax.ShapeAnd> listOfSequence) {
    this.ShapeAnd = ShapeAnd;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeOr)) {
      return false;
    }
    ShapeOr o = (ShapeOr) other;
    return java.util.Objects.equals(
      this.ShapeAnd,
      o.ShapeAnd) && java.util.Objects.equals(
      this.listOfSequence,
      o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ShapeAnd) + 3 * java.util.Objects.hashCode(listOfSequence);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShapeOr other) {
    int cmp = 0;
    cmp = ((Comparable) ShapeAnd).compareTo(other.ShapeAnd);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      listOfSequence.hashCode(),
      other.listOfSequence.hashCode());
  }
  
  public ShapeOr withShapeAnd(hydra.ext.io.shex.syntax.ShapeAnd ShapeAnd) {
    return new ShapeOr(ShapeAnd, listOfSequence);
  }
  
  public ShapeOr withListOfSequence(java.util.List<hydra.ext.io.shex.syntax.ShapeAnd> listOfSequence) {
    return new ShapeOr(ShapeAnd, listOfSequence);
  }
}
