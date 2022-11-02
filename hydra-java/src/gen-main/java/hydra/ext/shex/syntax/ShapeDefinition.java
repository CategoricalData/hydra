package hydra.ext.shex.syntax;

public class ShapeDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeDefinition");
  
  public final java.util.List<hydra.ext.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts;
  
  public final java.util.Optional<hydra.ext.shex.syntax.TripleExpression> tripleExpression;
  
  public final java.util.List<hydra.ext.shex.syntax.Annotation> listOfAnnotation;
  
  public final hydra.ext.shex.syntax.SemanticActions semanticActions;
  
  public ShapeDefinition (java.util.List<hydra.ext.shex.syntax.ShapeDefinition_ListOfAlts_Elmt> listOfAlts, java.util.Optional<hydra.ext.shex.syntax.TripleExpression> tripleExpression, java.util.List<hydra.ext.shex.syntax.Annotation> listOfAnnotation, hydra.ext.shex.syntax.SemanticActions semanticActions) {
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
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withTripleExpression(java.util.Optional<hydra.ext.shex.syntax.TripleExpression> tripleExpression) {
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withListOfAnnotation(java.util.List<hydra.ext.shex.syntax.Annotation> listOfAnnotation) {
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
  
  public ShapeDefinition withSemanticActions(hydra.ext.shex.syntax.SemanticActions semanticActions) {
    return new ShapeDefinition(listOfAlts, tripleExpression, listOfAnnotation, semanticActions);
  }
}