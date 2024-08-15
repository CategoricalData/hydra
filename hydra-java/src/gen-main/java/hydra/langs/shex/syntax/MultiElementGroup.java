// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class MultiElementGroup implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.MultiElementGroup");
  
  public static final hydra.core.Name FIELD_NAME_UNARY_TRIPLE_EXPR = new hydra.core.Name("unaryTripleExpr");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public static final hydra.core.Name FIELD_NAME_SEMI = new hydra.core.Name("semi");
  
  public final hydra.langs.shex.syntax.UnaryTripleExpr unaryTripleExpr;
  
  public final java.util.List<hydra.langs.shex.syntax.UnaryTripleExpr> listOfSequence;
  
  public final hydra.util.Opt<java.lang.Void> semi;
  
  public MultiElementGroup (hydra.langs.shex.syntax.UnaryTripleExpr unaryTripleExpr, java.util.List<hydra.langs.shex.syntax.UnaryTripleExpr> listOfSequence, hydra.util.Opt<java.lang.Void> semi) {
    java.util.Objects.requireNonNull((unaryTripleExpr));
    java.util.Objects.requireNonNull((listOfSequence));
    java.util.Objects.requireNonNull((semi));
    this.unaryTripleExpr = unaryTripleExpr;
    this.listOfSequence = listOfSequence;
    this.semi = semi;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementGroup)) {
      return false;
    }
    MultiElementGroup o = (MultiElementGroup) (other);
    return unaryTripleExpr.equals(o.unaryTripleExpr) && listOfSequence.equals(o.listOfSequence) && semi.equals(o.semi);
  }
  
  @Override
  public int hashCode() {
    return 2 * unaryTripleExpr.hashCode() + 3 * listOfSequence.hashCode() + 5 * semi.hashCode();
  }
  
  public MultiElementGroup withUnaryTripleExpr(hydra.langs.shex.syntax.UnaryTripleExpr unaryTripleExpr) {
    java.util.Objects.requireNonNull((unaryTripleExpr));
    return new MultiElementGroup(unaryTripleExpr, listOfSequence, semi);
  }
  
  public MultiElementGroup withListOfSequence(java.util.List<hydra.langs.shex.syntax.UnaryTripleExpr> listOfSequence) {
    java.util.Objects.requireNonNull((listOfSequence));
    return new MultiElementGroup(unaryTripleExpr, listOfSequence, semi);
  }
  
  public MultiElementGroup withSemi(hydra.util.Opt<java.lang.Void> semi) {
    java.util.Objects.requireNonNull((semi));
    return new MultiElementGroup(unaryTripleExpr, listOfSequence, semi);
  }
}