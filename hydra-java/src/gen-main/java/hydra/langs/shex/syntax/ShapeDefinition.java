// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class ShapeDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.ShapeDefinition");
  
  public final java.util.List<hydra.langs.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts;
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.TripleExpression> tripleExpression;
  
  public final java.util.List<hydra.langs.shex.syntax.Annotation> listOfAnnotation;
  
  public final hydra.langs.shex.syntax.SemanticActions semanticActions;
  
  public ShapeDefinition (java.util.List<hydra.langs.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts, hydra.util.Opt<hydra.langs.shex.syntax.TripleExpression> tripleExpression, java.util.List<hydra.langs.shex.syntax.Annotation> listOfAnnotation, hydra.langs.shex.syntax.SemanticActions semanticActions) {
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
  
  public ShapeDefinition withListOfAlts(java.util.List<hydra.langs.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts) {
    java.util.Objects.requireNonNull((listOfAlts));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withTripleExpression(hydra.util.Opt<hydra.langs.shex.syntax.TripleExpression> tripleExpression) {
    java.util.Objects.requireNonNull((tripleExpression));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withListOfAnnotation(java.util.List<hydra.langs.shex.syntax.Annotation> listOfAnnotation) {
    java.util.Objects.requireNonNull((listOfAnnotation));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withSemanticActions(hydra.langs.shex.syntax.SemanticActions semanticActions) {
    java.util.Objects.requireNonNull((semanticActions));
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
}