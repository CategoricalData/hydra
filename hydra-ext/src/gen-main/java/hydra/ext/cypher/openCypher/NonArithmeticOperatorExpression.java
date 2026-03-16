// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class NonArithmeticOperatorExpression implements Serializable, Comparable<NonArithmeticOperatorExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.NonArithmeticOperatorExpression");
  
  public static final hydra.core.Name ATOM = new hydra.core.Name("atom");
  
  public static final hydra.core.Name LISTS_AND_LOOKUPS = new hydra.core.Name("listsAndLookups");
  
  public static final hydra.core.Name LABELS = new hydra.core.Name("labels");
  
  public final hydra.ext.cypher.openCypher.Atom atom;
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup> listsAndLookups;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.NodeLabels> labels;
  
  public NonArithmeticOperatorExpression (hydra.ext.cypher.openCypher.Atom atom, hydra.util.ConsList<hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup> listsAndLookups, hydra.util.Maybe<hydra.ext.cypher.openCypher.NodeLabels> labels) {
    this.atom = atom;
    this.listsAndLookups = listsAndLookups;
    this.labels = labels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonArithmeticOperatorExpression)) {
      return false;
    }
    NonArithmeticOperatorExpression o = (NonArithmeticOperatorExpression) other;
    return java.util.Objects.equals(
      this.atom,
      o.atom) && java.util.Objects.equals(
      this.listsAndLookups,
      o.listsAndLookups) && java.util.Objects.equals(
      this.labels,
      o.labels);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(atom) + 3 * java.util.Objects.hashCode(listsAndLookups) + 5 * java.util.Objects.hashCode(labels);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NonArithmeticOperatorExpression other) {
    int cmp = 0;
    cmp = ((Comparable) atom).compareTo(other.atom);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) listsAndLookups).compareTo(other.listsAndLookups);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) labels).compareTo(other.labels);
  }
  
  public NonArithmeticOperatorExpression withAtom(hydra.ext.cypher.openCypher.Atom atom) {
    return new NonArithmeticOperatorExpression(atom, listsAndLookups, labels);
  }
  
  public NonArithmeticOperatorExpression withListsAndLookups(hydra.util.ConsList<hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup> listsAndLookups) {
    return new NonArithmeticOperatorExpression(atom, listsAndLookups, labels);
  }
  
  public NonArithmeticOperatorExpression withLabels(hydra.util.Maybe<hydra.ext.cypher.openCypher.NodeLabels> labels) {
    return new NonArithmeticOperatorExpression(atom, listsAndLookups, labels);
  }
}
