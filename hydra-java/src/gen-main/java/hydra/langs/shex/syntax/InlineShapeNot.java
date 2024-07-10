// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class InlineShapeNot implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.InlineShapeNot");
  
  public final java.util.Optional<java.lang.Void> nOT;
  
  public final hydra.langs.shex.syntax.InlineShapeAtom inlineShapeAtom;
  
  public InlineShapeNot (java.util.Optional<java.lang.Void> nOT, hydra.langs.shex.syntax.InlineShapeAtom inlineShapeAtom) {
    if (nOT == null) {
      throw new IllegalArgumentException("null value for 'nOT' argument");
    }
    if (inlineShapeAtom == null) {
      throw new IllegalArgumentException("null value for 'inlineShapeAtom' argument");
    }
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
  
  public InlineShapeNot withNOT(java.util.Optional<java.lang.Void> nOT) {
    if (nOT == null) {
      throw new IllegalArgumentException("null value for 'nOT' argument");
    }
    return new InlineShapeNot(nOT, inlineShapeAtom);
  }
  
  public InlineShapeNot withInlineShapeAtom(hydra.langs.shex.syntax.InlineShapeAtom inlineShapeAtom) {
    if (inlineShapeAtom == null) {
      throw new IllegalArgumentException("null value for 'inlineShapeAtom' argument");
    }
    return new InlineShapeNot(nOT, inlineShapeAtom);
  }
}