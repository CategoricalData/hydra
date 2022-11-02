package hydra.ext.shex.syntax;

public class InlineShapeNot {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeNot");
  
  public final java.util.Optional<java.lang.Void> nOT;
  
  public final hydra.ext.shex.syntax.InlineShapeAtom inlineShapeAtom;
  
  public InlineShapeNot (java.util.Optional<java.lang.Void> nOT, hydra.ext.shex.syntax.InlineShapeAtom inlineShapeAtom) {
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
    return new InlineShapeNot(nOT, inlineShapeAtom);
  }
  
  public InlineShapeNot withInlineShapeAtom(hydra.ext.shex.syntax.InlineShapeAtom inlineShapeAtom) {
    return new InlineShapeNot(nOT, inlineShapeAtom);
  }
}