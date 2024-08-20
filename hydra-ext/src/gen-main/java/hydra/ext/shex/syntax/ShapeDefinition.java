// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class ShapeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_ALTS = new hydra.core.Name("listOfAlts");
  
  public static final hydra.core.Name FIELD_NAME_TRIPLE_EXPRESSION = new hydra.core.Name("tripleExpression");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_ANNOTATION = new hydra.core.Name("listOfAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_SEMANTIC_ACTIONS = new hydra.core.Name("semanticActions");
  
  public final java.util.List<hydra.ext.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts;
  
  public final hydra.util.Opt<hydra.ext.shex.syntax.TripleExpression> tripleExpression;
  
  public final java.util.List<hydra.ext.shex.syntax.Annotation> listOfAnnotation;
  
  public final hydra.ext.shex.syntax.SemanticActions semanticActions;
  
  public ShapeDefinition (java.util.List<hydra.ext.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts, hydra.util.Opt<hydra.ext.shex.syntax.TripleExpression> tripleExpression, java.util.List<hydra.ext.shex.syntax.Annotation> listOfAnnotation, hydra.ext.shex.syntax.SemanticActions semanticActions) {
    java.util.Objects.requireNonNull((listOfAlts));
    java.util.Objects.requireNonNull((tripleExpression));
    java.util.Objects.requireNonNull((listOfAnnotation));
    java.util.Objects.requireNonNull((semanticActions));
    this.listOfAlts = listOfAlts;
    this.tripleExpression = tripleExpression;
    this.listOfAnnotation = listOfAnnotation;
    this.semanticActions = semanticActions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeDefinition)) {
      return false;
    }
    ShapeDefinition o = (ShapeDefinition) (other);
    return listOfAlts.equals(o.listOfAlts) && tripleExpression.equals(o.tripleExpression) && listOfAnnotation.equals(o.listOfAnnotation) && semanticActions.equals(o.semanticActions);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode() + 3 * tripleExpression.hashCode() + 5 * listOfAnnotation.hashCode() + 7 * semanticActions.hashCode();
  }
  
  public ShapeDefinition withListOfAlts(java.util.List<hydra.ext.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts) {
    java.util.Objects.requireNonNull((listOfAlts));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withTripleExpression(hydra.util.Opt<hydra.ext.shex.syntax.TripleExpression> tripleExpression) {
    java.util.Objects.requireNonNull((tripleExpression));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withListOfAnnotation(java.util.List<hydra.ext.shex.syntax.Annotation> listOfAnnotation) {
    java.util.Objects.requireNonNull((listOfAnnotation));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withSemanticActions(hydra.ext.shex.syntax.SemanticActions semanticActions) {
    java.util.Objects.requireNonNull((semanticActions));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
}