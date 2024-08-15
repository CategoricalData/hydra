// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class InlineShapeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.InlineShapeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_ALTS = new hydra.core.Name("listOfAlts");
  
  public static final hydra.core.Name FIELD_NAME_TRIPLE_EXPRESSION = new hydra.core.Name("tripleExpression");
  
  public final java.util.List<hydra.langs.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt> listOfAlts;
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.TripleExpression> tripleExpression;
  
  public InlineShapeDefinition (java.util.List<hydra.langs.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt> listOfAlts, hydra.util.Opt<hydra.langs.shex.syntax.TripleExpression> tripleExpression) {
    java.util.Objects.requireNonNull((listOfAlts));
    java.util.Objects.requireNonNull((tripleExpression));
    this.listOfAlts = listOfAlts;
    this.tripleExpression = tripleExpression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeDefinition)) {
      return false;
    }
    InlineShapeDefinition o = (InlineShapeDefinition) (other);
    return listOfAlts.equals(o.listOfAlts) && tripleExpression.equals(o.tripleExpression);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode() + 3 * tripleExpression.hashCode();
  }
  
  public InlineShapeDefinition withListOfAlts(java.util.List<hydra.langs.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt> listOfAlts) {
    java.util.Objects.requireNonNull((listOfAlts));
    return new InlineShapeDefinition(listOfAlts, tripleExpression);
  }
  
  public InlineShapeDefinition withTripleExpression(hydra.util.Opt<hydra.langs.shex.syntax.TripleExpression> tripleExpression) {
    java.util.Objects.requireNonNull((tripleExpression));
    return new InlineShapeDefinition(listOfAlts, tripleExpression);
  }
}