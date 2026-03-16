// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class MultiElementGroup implements Serializable, Comparable<MultiElementGroup> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementGroup");
  
  public static final hydra.core.Name UNARY_TRIPLE_EXPR = new hydra.core.Name("UnaryTripleExpr");
  
  public static final hydra.core.Name LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public static final hydra.core.Name SEMI = new hydra.core.Name("Semi");
  
  public final hydra.ext.io.shex.syntax.UnaryTripleExpr UnaryTripleExpr;
  
  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.UnaryTripleExpr> listOfSequence;
  
  public final hydra.util.Maybe<java.lang.Void> Semi;
  
  public MultiElementGroup (hydra.ext.io.shex.syntax.UnaryTripleExpr UnaryTripleExpr, hydra.util.ConsList<hydra.ext.io.shex.syntax.UnaryTripleExpr> listOfSequence, hydra.util.Maybe<java.lang.Void> Semi) {
    this.UnaryTripleExpr = UnaryTripleExpr;
    this.listOfSequence = listOfSequence;
    this.Semi = Semi;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementGroup)) {
      return false;
    }
    MultiElementGroup o = (MultiElementGroup) other;
    return java.util.Objects.equals(
      this.UnaryTripleExpr,
      o.UnaryTripleExpr) && java.util.Objects.equals(
      this.listOfSequence,
      o.listOfSequence) && java.util.Objects.equals(
      this.Semi,
      o.Semi);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(UnaryTripleExpr) + 3 * java.util.Objects.hashCode(listOfSequence) + 5 * java.util.Objects.hashCode(Semi);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MultiElementGroup other) {
    int cmp = 0;
    cmp = ((Comparable) UnaryTripleExpr).compareTo(other.UnaryTripleExpr);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) listOfSequence).compareTo(other.listOfSequence);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) Semi).compareTo(other.Semi);
  }
  
  public MultiElementGroup withUnaryTripleExpr(hydra.ext.io.shex.syntax.UnaryTripleExpr UnaryTripleExpr) {
    return new MultiElementGroup(UnaryTripleExpr, listOfSequence, Semi);
  }
  
  public MultiElementGroup withListOfSequence(hydra.util.ConsList<hydra.ext.io.shex.syntax.UnaryTripleExpr> listOfSequence) {
    return new MultiElementGroup(UnaryTripleExpr, listOfSequence, Semi);
  }
  
  public MultiElementGroup withSemi(hydra.util.Maybe<java.lang.Void> Semi) {
    return new MultiElementGroup(UnaryTripleExpr, listOfSequence, Semi);
  }
}
