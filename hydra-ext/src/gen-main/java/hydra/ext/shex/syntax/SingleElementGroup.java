// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class SingleElementGroup implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.SingleElementGroup");
  
  public static final hydra.core.Name FIELD_NAME_UNARY_TRIPLE_EXPR = new hydra.core.Name("unaryTripleExpr");
  
  public static final hydra.core.Name FIELD_NAME_SEMI = new hydra.core.Name("semi");
  
  public final hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr;
  
  public final hydra.util.Opt<java.lang.Void> semi;
  
  public SingleElementGroup (hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr, hydra.util.Opt<java.lang.Void> semi) {
    java.util.Objects.requireNonNull((unaryTripleExpr));
    java.util.Objects.requireNonNull((semi));
    this.unaryTripleExpr = unaryTripleExpr;
    this.semi = semi;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleElementGroup)) {
      return false;
    }
    SingleElementGroup o = (SingleElementGroup) (other);
    return unaryTripleExpr.equals(o.unaryTripleExpr) && semi.equals(o.semi);
  }
  
  @Override
  public int hashCode() {
    return 2 * unaryTripleExpr.hashCode() + 3 * semi.hashCode();
  }
  
  public SingleElementGroup withUnaryTripleExpr(hydra.ext.shex.syntax.UnaryTripleExpr unaryTripleExpr) {
    java.util.Objects.requireNonNull((unaryTripleExpr));
    return new SingleElementGroup(unaryTripleExpr, semi);
  }
  
  public SingleElementGroup withSemi(hydra.util.Opt<java.lang.Void> semi) {
    java.util.Objects.requireNonNull((semi));
    return new SingleElementGroup(unaryTripleExpr, semi);
  }
}
