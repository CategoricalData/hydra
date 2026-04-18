// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class BracketedTripleExpr implements Serializable, Comparable<BracketedTripleExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.BracketedTripleExpr");

  public static final hydra.core.Name INNER_TRIPLE_EXPR = new hydra.core.Name("InnerTripleExpr");

  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("Cardinality");

  public static final hydra.core.Name LIST_OF_ANNOTATION = new hydra.core.Name("listOfAnnotation");

  public static final hydra.core.Name SEMANTIC_ACTIONS = new hydra.core.Name("SemanticActions");

  public final hydra.shex.syntax.InnerTripleExpr InnerTripleExpr;

  public final hydra.util.Maybe<hydra.shex.syntax.Cardinality> Cardinality;

  public final java.util.List<hydra.shex.syntax.Annotation> listOfAnnotation;

  public final hydra.shex.syntax.SemanticActions SemanticActions;

  public BracketedTripleExpr (hydra.shex.syntax.InnerTripleExpr InnerTripleExpr, hydra.util.Maybe<hydra.shex.syntax.Cardinality> Cardinality, java.util.List<hydra.shex.syntax.Annotation> listOfAnnotation, hydra.shex.syntax.SemanticActions SemanticActions) {
    this.InnerTripleExpr = InnerTripleExpr;
    this.Cardinality = Cardinality;
    this.listOfAnnotation = listOfAnnotation;
    this.SemanticActions = SemanticActions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BracketedTripleExpr)) {
      return false;
    }
    BracketedTripleExpr o = (BracketedTripleExpr) other;
    return java.util.Objects.equals(
      this.InnerTripleExpr,
      o.InnerTripleExpr) && java.util.Objects.equals(
      this.Cardinality,
      o.Cardinality) && java.util.Objects.equals(
      this.listOfAnnotation,
      o.listOfAnnotation) && java.util.Objects.equals(
      this.SemanticActions,
      o.SemanticActions);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(InnerTripleExpr) + 3 * java.util.Objects.hashCode(Cardinality) + 5 * java.util.Objects.hashCode(listOfAnnotation) + 7 * java.util.Objects.hashCode(SemanticActions);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BracketedTripleExpr other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      InnerTripleExpr,
      other.InnerTripleExpr);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      Cardinality,
      other.Cardinality);
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

  public BracketedTripleExpr withInnerTripleExpr(hydra.shex.syntax.InnerTripleExpr InnerTripleExpr) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }

  public BracketedTripleExpr withCardinality(hydra.util.Maybe<hydra.shex.syntax.Cardinality> Cardinality) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }

  public BracketedTripleExpr withListOfAnnotation(java.util.List<hydra.shex.syntax.Annotation> listOfAnnotation) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }

  public BracketedTripleExpr withSemanticActions(hydra.shex.syntax.SemanticActions SemanticActions) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }
}
