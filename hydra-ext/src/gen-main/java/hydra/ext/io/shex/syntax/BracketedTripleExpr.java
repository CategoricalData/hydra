// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class BracketedTripleExpr implements Serializable, Comparable<BracketedTripleExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.BracketedTripleExpr");
  
  public static final hydra.core.Name INNER_TRIPLE_EXPR = new hydra.core.Name("InnerTripleExpr");
  
  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("Cardinality");
  
  public static final hydra.core.Name LIST_OF_ANNOTATION = new hydra.core.Name("listOfAnnotation");
  
  public static final hydra.core.Name SEMANTIC_ACTIONS = new hydra.core.Name("SemanticActions");
  
  public final hydra.ext.io.shex.syntax.InnerTripleExpr InnerTripleExpr;
  
  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.Cardinality> Cardinality;
  
  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation;
  
  public final hydra.ext.io.shex.syntax.SemanticActions SemanticActions;
  
  public BracketedTripleExpr (hydra.ext.io.shex.syntax.InnerTripleExpr InnerTripleExpr, hydra.util.Maybe<hydra.ext.io.shex.syntax.Cardinality> Cardinality, hydra.util.ConsList<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation, hydra.ext.io.shex.syntax.SemanticActions SemanticActions) {
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
    cmp = ((Comparable) InnerTripleExpr).compareTo(other.InnerTripleExpr);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      Cardinality.hashCode(),
      other.Cardinality.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      listOfAnnotation.hashCode(),
      other.listOfAnnotation.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) SemanticActions).compareTo(other.SemanticActions);
  }
  
  public BracketedTripleExpr withInnerTripleExpr(hydra.ext.io.shex.syntax.InnerTripleExpr InnerTripleExpr) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }
  
  public BracketedTripleExpr withCardinality(hydra.util.Maybe<hydra.ext.io.shex.syntax.Cardinality> Cardinality) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }
  
  public BracketedTripleExpr withListOfAnnotation(hydra.util.ConsList<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }
  
  public BracketedTripleExpr withSemanticActions(hydra.ext.io.shex.syntax.SemanticActions SemanticActions) {
    return new BracketedTripleExpr(InnerTripleExpr, Cardinality, listOfAnnotation, SemanticActions);
  }
}
