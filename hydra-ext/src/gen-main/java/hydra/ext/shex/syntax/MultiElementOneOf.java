// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class MultiElementOneOf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.MultiElementOneOf");
  
  public static final hydra.core.Name FIELD_NAME_GROUP_TRIPLE_EXPR = new hydra.core.Name("groupTripleExpr");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr;
  
  public final java.util.List<hydra.ext.shex.syntax.GroupTripleExpr> listOfSequence;
  
  public MultiElementOneOf (hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr, java.util.List<hydra.ext.shex.syntax.GroupTripleExpr> listOfSequence) {
    java.util.Objects.requireNonNull((groupTripleExpr));
    java.util.Objects.requireNonNull((listOfSequence));
    this.groupTripleExpr = groupTripleExpr;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementOneOf)) {
      return false;
    }
    MultiElementOneOf o = (MultiElementOneOf) (other);
    return groupTripleExpr.equals(o.groupTripleExpr) && listOfSequence.equals(o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * groupTripleExpr.hashCode() + 3 * listOfSequence.hashCode();
  }
  
  public MultiElementOneOf withGroupTripleExpr(hydra.ext.shex.syntax.GroupTripleExpr groupTripleExpr) {
    java.util.Objects.requireNonNull((groupTripleExpr));
    return new MultiElementOneOf(groupTripleExpr, listOfSequence);
  }
  
  public MultiElementOneOf withListOfSequence(java.util.List<hydra.ext.shex.syntax.GroupTripleExpr> listOfSequence) {
    java.util.Objects.requireNonNull((listOfSequence));
    return new MultiElementOneOf(groupTripleExpr, listOfSequence);
  }
}