// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShapeAnd implements Serializable, Comparable<ShapeAnd> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAnd");

  public static final hydra.core.Name SHAPE_NOT = new hydra.core.Name("ShapeNot");

  public static final hydra.core.Name LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");

  public final hydra.ext.io.shex.syntax.ShapeNot ShapeNot;

  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.ShapeNot> listOfSequence;

  public ShapeAnd (hydra.ext.io.shex.syntax.ShapeNot ShapeNot, hydra.util.ConsList<hydra.ext.io.shex.syntax.ShapeNot> listOfSequence) {
    this.ShapeNot = ShapeNot;
    this.listOfSequence = listOfSequence;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeAnd)) {
      return false;
    }
    ShapeAnd o = (ShapeAnd) other;
    return java.util.Objects.equals(
      this.ShapeNot,
      o.ShapeNot) && java.util.Objects.equals(
      this.listOfSequence,
      o.listOfSequence);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ShapeNot) + 3 * java.util.Objects.hashCode(listOfSequence);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShapeAnd other) {
    int cmp = 0;
    cmp = ((Comparable) ShapeNot).compareTo(other.ShapeNot);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) listOfSequence).compareTo(other.listOfSequence);
  }

  public ShapeAnd withShapeNot(hydra.ext.io.shex.syntax.ShapeNot ShapeNot) {
    return new ShapeAnd(ShapeNot, listOfSequence);
  }

  public ShapeAnd withListOfSequence(hydra.util.ConsList<hydra.ext.io.shex.syntax.ShapeNot> listOfSequence) {
    return new ShapeAnd(ShapeNot, listOfSequence);
  }
}
