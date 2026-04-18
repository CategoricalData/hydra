// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class InlineShapeNot implements Serializable, Comparable<InlineShapeNot> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.InlineShapeNot");

  public static final hydra.core.Name N_O_T = new hydra.core.Name("NOT");

  public static final hydra.core.Name INLINE_SHAPE_ATOM = new hydra.core.Name("InlineShapeAtom");

  public final hydra.util.Maybe<java.lang.Void> NOT;

  public final hydra.shex.syntax.InlineShapeAtom InlineShapeAtom;

  public InlineShapeNot (hydra.util.Maybe<java.lang.Void> NOT, hydra.shex.syntax.InlineShapeAtom InlineShapeAtom) {
    this.NOT = NOT;
    this.InlineShapeAtom = InlineShapeAtom;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeNot)) {
      return false;
    }
    InlineShapeNot o = (InlineShapeNot) other;
    return java.util.Objects.equals(
      this.NOT,
      o.NOT) && java.util.Objects.equals(
      this.InlineShapeAtom,
      o.InlineShapeAtom);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(NOT) + 3 * java.util.Objects.hashCode(InlineShapeAtom);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InlineShapeNot other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      NOT,
      other.NOT);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      InlineShapeAtom,
      other.InlineShapeAtom);
  }

  public InlineShapeNot withNOT(hydra.util.Maybe<java.lang.Void> NOT) {
    return new InlineShapeNot(NOT, InlineShapeAtom);
  }

  public InlineShapeNot withInlineShapeAtom(hydra.shex.syntax.InlineShapeAtom InlineShapeAtom) {
    return new InlineShapeNot(NOT, InlineShapeAtom);
  }
}
