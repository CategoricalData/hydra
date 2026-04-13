// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShapeDefinition implements Serializable, Comparable<ShapeDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeDefinition");

  public static final hydra.core.Name LIST_OF_ALTS = new hydra.core.Name("listOfAlts");

  public static final hydra.core.Name TRIPLE_EXPRESSION = new hydra.core.Name("TripleExpression");

  public static final hydra.core.Name LIST_OF_ANNOTATION = new hydra.core.Name("listOfAnnotation");

  public static final hydra.core.Name SEMANTIC_ACTIONS = new hydra.core.Name("SemanticActions");

  public final java.util.List<hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts;

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.TripleExpression> TripleExpression;

  public final java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation;

  public final hydra.ext.io.shex.syntax.SemanticActions SemanticActions;

  public ShapeDefinition (java.util.List<hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts, hydra.util.Maybe<hydra.ext.io.shex.syntax.TripleExpression> TripleExpression, java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation, hydra.ext.io.shex.syntax.SemanticActions SemanticActions) {
    this.listOfAlts = listOfAlts;
    this.TripleExpression = TripleExpression;
    this.listOfAnnotation = listOfAnnotation;
    this.SemanticActions = SemanticActions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeDefinition)) {
      return false;
    }
    ShapeDefinition o = (ShapeDefinition) other;
    return java.util.Objects.equals(
      this.listOfAlts,
      o.listOfAlts) && java.util.Objects.equals(
      this.TripleExpression,
      o.TripleExpression) && java.util.Objects.equals(
      this.listOfAnnotation,
      o.listOfAnnotation) && java.util.Objects.equals(
      this.SemanticActions,
      o.SemanticActions);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listOfAlts) + 3 * java.util.Objects.hashCode(TripleExpression) + 5 * java.util.Objects.hashCode(listOfAnnotation) + 7 * java.util.Objects.hashCode(SemanticActions);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShapeDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      listOfAlts,
      other.listOfAlts);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      TripleExpression,
      other.TripleExpression);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      listOfAnnotation,
      other.listOfAnnotation);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      SemanticActions,
      other.SemanticActions);
  }

  public ShapeDefinition withListOfAlts(java.util.List<hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts) {
    return new ShapeDefinition(listOfAlts, TripleExpression, listOfAnnotation, SemanticActions);
  }

  public ShapeDefinition withTripleExpression(hydra.util.Maybe<hydra.ext.io.shex.syntax.TripleExpression> TripleExpression) {
    return new ShapeDefinition(listOfAlts, TripleExpression, listOfAnnotation, SemanticActions);
  }

  public ShapeDefinition withListOfAnnotation(java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation) {
    return new ShapeDefinition(listOfAlts, TripleExpression, listOfAnnotation, SemanticActions);
  }

  public ShapeDefinition withSemanticActions(hydra.ext.io.shex.syntax.SemanticActions SemanticActions) {
    return new ShapeDefinition(listOfAlts, TripleExpression, listOfAnnotation, SemanticActions);
  }
}
