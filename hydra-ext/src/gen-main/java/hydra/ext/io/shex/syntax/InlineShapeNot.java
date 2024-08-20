// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class InlineShapeNot implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.InlineShapeNot");
  
  public static final hydra.core.Name FIELD_NAME_N_O_T = new hydra.core.Name("nOT");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_SHAPE_ATOM = new hydra.core.Name("inlineShapeAtom");
  
  public final hydra.util.Opt<java.lang.Void> nOT;
  
  public final hydra.ext.io.shex.syntax.InlineShapeAtom inlineShapeAtom;
  
  public InlineShapeNot (hydra.util.Opt<java.lang.Void> nOT, hydra.ext.io.shex.syntax.InlineShapeAtom inlineShapeAtom) {
    java.util.Objects.requireNonNull((nOT));
    java.util.Objects.requireNonNull((inlineShapeAtom));
    this.nOT = nOT;
    this.inlineShapeAtom = inlineShapeAtom;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeNot)) {
      return false;
    }
    InlineShapeNot o = (InlineShapeNot) (other);
    return nOT.equals(o.nOT) && inlineShapeAtom.equals(o.inlineShapeAtom);
  }
  
  @Override
  public int hashCode() {
    return 2 * nOT.hashCode() + 3 * inlineShapeAtom.hashCode();
  }
  
  public InlineShapeNot withNOT(hydra.util.Opt<java.lang.Void> nOT) {
    java.util.Objects.requireNonNull((nOT));
    return new InlineShapeNot(nOT, inlineShapeAtom);
  }
  
  public InlineShapeNot withInlineShapeAtom(hydra.ext.io.shex.syntax.InlineShapeAtom inlineShapeAtom) {
    java.util.Objects.requireNonNull((inlineShapeAtom));
    return new InlineShapeNot(nOT, inlineShapeAtom);
  }
}