// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class BracketedTripleExpr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.BracketedTripleExpr");
  
  public static final hydra.core.Name FIELD_NAME_INNER_TRIPLE_EXPR = new hydra.core.Name("innerTripleExpr");
  
  public static final hydra.core.Name FIELD_NAME_CARDINALITY = new hydra.core.Name("cardinality");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_ANNOTATION = new hydra.core.Name("listOfAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_SEMANTIC_ACTIONS = new hydra.core.Name("semanticActions");
  
  public final hydra.ext.io.shex.syntax.InnerTripleExpr innerTripleExpr;
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.Cardinality> cardinality;
  
  public final java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation;
  
  public final hydra.ext.io.shex.syntax.SemanticActions semanticActions;
  
  public BracketedTripleExpr (hydra.ext.io.shex.syntax.InnerTripleExpr innerTripleExpr, hydra.util.Opt<hydra.ext.io.shex.syntax.Cardinality> cardinality, java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation, hydra.ext.io.shex.syntax.SemanticActions semanticActions) {
    java.util.Objects.requireNonNull((innerTripleExpr));
    java.util.Objects.requireNonNull((cardinality));
    java.util.Objects.requireNonNull((listOfAnnotation));
    java.util.Objects.requireNonNull((semanticActions));
    this.innerTripleExpr = innerTripleExpr;
    this.cardinality = cardinality;
    this.listOfAnnotation = listOfAnnotation;
    this.semanticActions = semanticActions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BracketedTripleExpr)) {
      return false;
    }
    BracketedTripleExpr o = (BracketedTripleExpr) (other);
    return innerTripleExpr.equals(o.innerTripleExpr) && cardinality.equals(o.cardinality) && listOfAnnotation.equals(o.listOfAnnotation) && semanticActions.equals(o.semanticActions);
  }
  
  @Override
  public int hashCode() {
    return 2 * innerTripleExpr.hashCode() + 3 * cardinality.hashCode() + 5 * listOfAnnotation.hashCode() + 7 * semanticActions.hashCode();
  }
  
  public BracketedTripleExpr withInnerTripleExpr(hydra.ext.io.shex.syntax.InnerTripleExpr innerTripleExpr) {
    java.util.Objects.requireNonNull((innerTripleExpr));
    return new BracketedTripleExpr(innerTripleExpr, cardinality, listOfAnnotation, semanticActions);
  }
  
  public BracketedTripleExpr withCardinality(hydra.util.Opt<hydra.ext.io.shex.syntax.Cardinality> cardinality) {
    java.util.Objects.requireNonNull((cardinality));
    return new BracketedTripleExpr(innerTripleExpr, cardinality, listOfAnnotation, semanticActions);
  }
  
  public BracketedTripleExpr withListOfAnnotation(java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation) {
    java.util.Objects.requireNonNull((listOfAnnotation));
    return new BracketedTripleExpr(innerTripleExpr, cardinality, listOfAnnotation, semanticActions);
  }
  
  public BracketedTripleExpr withSemanticActions(hydra.ext.io.shex.syntax.SemanticActions semanticActions) {
    java.util.Objects.requireNonNull((semanticActions));
    return new BracketedTripleExpr(innerTripleExpr, cardinality, listOfAnnotation, semanticActions);
  }
}