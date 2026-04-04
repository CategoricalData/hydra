// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShapeNot implements Serializable, Comparable<ShapeNot> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeNot");

  public static final hydra.core.Name N_O_T = new hydra.core.Name("NOT");

  public static final hydra.core.Name SHAPE_ATOM = new hydra.core.Name("ShapeAtom");

  public final hydra.util.Maybe<java.lang.Void> NOT;

  public final hydra.ext.io.shex.syntax.ShapeAtom ShapeAtom;

  public ShapeNot (hydra.util.Maybe<java.lang.Void> NOT, hydra.ext.io.shex.syntax.ShapeAtom ShapeAtom) {
    this.NOT = NOT;
    this.ShapeAtom = ShapeAtom;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeNot)) {
      return false;
    }
    ShapeNot o = (ShapeNot) other;
    return java.util.Objects.equals(
      this.NOT,
      o.NOT) && java.util.Objects.equals(
      this.ShapeAtom,
      o.ShapeAtom);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(NOT) + 3 * java.util.Objects.hashCode(ShapeAtom);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShapeNot other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      NOT,
      other.NOT);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      ShapeAtom,
      other.ShapeAtom);
  }

  public ShapeNot withNOT(hydra.util.Maybe<java.lang.Void> NOT) {
    return new ShapeNot(NOT, ShapeAtom);
  }

  public ShapeNot withShapeAtom(hydra.ext.io.shex.syntax.ShapeAtom ShapeAtom) {
    return new ShapeNot(NOT, ShapeAtom);
  }
}
