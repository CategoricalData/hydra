// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class ShapeNot implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeNot");
  
  public static final hydra.core.Name FIELD_NAME_N_O_T = new hydra.core.Name("nOT");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_ATOM = new hydra.core.Name("shapeAtom");
  
  public final hydra.util.Opt<java.lang.Void> nOT;
  
  public final hydra.ext.shex.syntax.ShapeAtom shapeAtom;
  
  public ShapeNot (hydra.util.Opt<java.lang.Void> nOT, hydra.ext.shex.syntax.ShapeAtom shapeAtom) {
    java.util.Objects.requireNonNull((nOT));
    java.util.Objects.requireNonNull((shapeAtom));
    this.nOT = nOT;
    this.shapeAtom = shapeAtom;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeNot)) {
      return false;
    }
    ShapeNot o = (ShapeNot) (other);
    return nOT.equals(o.nOT) && shapeAtom.equals(o.shapeAtom);
  }
  
  @Override
  public int hashCode() {
    return 2 * nOT.hashCode() + 3 * shapeAtom.hashCode();
  }
  
  public ShapeNot withNOT(hydra.util.Opt<java.lang.Void> nOT) {
    java.util.Objects.requireNonNull((nOT));
    return new ShapeNot(nOT, shapeAtom);
  }
  
  public ShapeNot withShapeAtom(hydra.ext.shex.syntax.ShapeAtom shapeAtom) {
    java.util.Objects.requireNonNull((shapeAtom));
    return new ShapeNot(nOT, shapeAtom);
  }
}