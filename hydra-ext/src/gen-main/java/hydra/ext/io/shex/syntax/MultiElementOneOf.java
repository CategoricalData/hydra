// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class MultiElementOneOf implements Serializable, Comparable<MultiElementOneOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementOneOf");
  
  public static final hydra.core.Name GROUP_TRIPLE_EXPR = new hydra.core.Name("GroupTripleExpr");
  
  public static final hydra.core.Name LIST_OF_SEQUENCE = new hydra.core.Name("listOfSequence");
  
  public final hydra.ext.io.shex.syntax.GroupTripleExpr GroupTripleExpr;
  
  public final java.util.List<hydra.ext.io.shex.syntax.GroupTripleExpr> listOfSequence;
  
  public MultiElementOneOf (hydra.ext.io.shex.syntax.GroupTripleExpr GroupTripleExpr, java.util.List<hydra.ext.io.shex.syntax.GroupTripleExpr> listOfSequence) {
    this.GroupTripleExpr = GroupTripleExpr;
    this.listOfSequence = listOfSequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiElementOneOf)) {
      return false;
    }
    MultiElementOneOf o = (MultiElementOneOf) other;
    return java.util.Objects.equals(
      this.GroupTripleExpr,
      o.GroupTripleExpr) && java.util.Objects.equals(
      this.listOfSequence,
      o.listOfSequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(GroupTripleExpr) + 3 * java.util.Objects.hashCode(listOfSequence);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MultiElementOneOf other) {
    int cmp = 0;
    cmp = ((Comparable) GroupTripleExpr).compareTo(other.GroupTripleExpr);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      listOfSequence.hashCode(),
      other.listOfSequence.hashCode());
  }
  
  public MultiElementOneOf withGroupTripleExpr(hydra.ext.io.shex.syntax.GroupTripleExpr GroupTripleExpr) {
    return new MultiElementOneOf(GroupTripleExpr, listOfSequence);
  }
  
  public MultiElementOneOf withListOfSequence(java.util.List<hydra.ext.io.shex.syntax.GroupTripleExpr> listOfSequence) {
    return new MultiElementOneOf(GroupTripleExpr, listOfSequence);
  }
}
