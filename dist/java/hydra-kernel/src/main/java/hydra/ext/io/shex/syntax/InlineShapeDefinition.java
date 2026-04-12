// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class InlineShapeDefinition implements Serializable, Comparable<InlineShapeDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeDefinition");

  public static final hydra.core.Name LIST_OF_ALTS = new hydra.core.Name("listOfAlts");

  public static final hydra.core.Name TRIPLE_EXPRESSION = new hydra.core.Name("TripleExpression");

  public final java.util.List<hydra.ext.io.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt> listOfAlts;

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.TripleExpression> TripleExpression;

  public InlineShapeDefinition (java.util.List<hydra.ext.io.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt> listOfAlts, hydra.util.Maybe<hydra.ext.io.shex.syntax.TripleExpression> TripleExpression) {
    this.listOfAlts = listOfAlts;
    this.TripleExpression = TripleExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeDefinition)) {
      return false;
    }
    InlineShapeDefinition o = (InlineShapeDefinition) other;
    return java.util.Objects.equals(
      this.listOfAlts,
      o.listOfAlts) && java.util.Objects.equals(
      this.TripleExpression,
      o.TripleExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listOfAlts) + 3 * java.util.Objects.hashCode(TripleExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InlineShapeDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      listOfAlts,
      other.listOfAlts);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      TripleExpression,
      other.TripleExpression);
  }

  public InlineShapeDefinition withListOfAlts(java.util.List<hydra.ext.io.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt> listOfAlts) {
    return new InlineShapeDefinition(listOfAlts, TripleExpression);
  }

  public InlineShapeDefinition withTripleExpression(hydra.util.Maybe<hydra.ext.io.shex.syntax.TripleExpression> TripleExpression) {
    return new InlineShapeDefinition(listOfAlts, TripleExpression);
  }
}
