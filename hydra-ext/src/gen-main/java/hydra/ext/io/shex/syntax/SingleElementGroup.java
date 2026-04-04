// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class SingleElementGroup implements Serializable, Comparable<SingleElementGroup> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.SingleElementGroup");

  public static final hydra.core.Name UNARY_TRIPLE_EXPR = new hydra.core.Name("UnaryTripleExpr");

  public static final hydra.core.Name SEMI = new hydra.core.Name("Semi");

  public final hydra.ext.io.shex.syntax.UnaryTripleExpr UnaryTripleExpr;

  public final hydra.util.Maybe<java.lang.Void> Semi;

  public SingleElementGroup (hydra.ext.io.shex.syntax.UnaryTripleExpr UnaryTripleExpr, hydra.util.Maybe<java.lang.Void> Semi) {
    this.UnaryTripleExpr = UnaryTripleExpr;
    this.Semi = Semi;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleElementGroup)) {
      return false;
    }
    SingleElementGroup o = (SingleElementGroup) other;
    return java.util.Objects.equals(
      this.UnaryTripleExpr,
      o.UnaryTripleExpr) && java.util.Objects.equals(
      this.Semi,
      o.Semi);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(UnaryTripleExpr) + 3 * java.util.Objects.hashCode(Semi);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SingleElementGroup other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      UnaryTripleExpr,
      other.UnaryTripleExpr);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      Semi,
      other.Semi);
  }

  public SingleElementGroup withUnaryTripleExpr(hydra.ext.io.shex.syntax.UnaryTripleExpr UnaryTripleExpr) {
    return new SingleElementGroup(UnaryTripleExpr, Semi);
  }

  public SingleElementGroup withSemi(hydra.util.Maybe<java.lang.Void> Semi) {
    return new SingleElementGroup(UnaryTripleExpr, Semi);
  }
}
