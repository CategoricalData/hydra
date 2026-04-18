// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ElementPatternFiller implements Serializable, Comparable<ElementPatternFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ElementPatternFiller");

  public static final hydra.core.Name VARIABLE_DECLARATION = new hydra.core.Name("variableDeclaration");

  public static final hydra.core.Name IS_LABEL_EXPRESSION = new hydra.core.Name("isLabelExpression");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  public final hydra.util.Maybe<openGql.grammar.ElementVariableDeclaration> variableDeclaration;

  public final hydra.util.Maybe<openGql.grammar.IsLabelExpression> isLabelExpression;

  public final hydra.util.Maybe<openGql.grammar.ElementPatternPredicate> predicate;

  public ElementPatternFiller (hydra.util.Maybe<openGql.grammar.ElementVariableDeclaration> variableDeclaration, hydra.util.Maybe<openGql.grammar.IsLabelExpression> isLabelExpression, hydra.util.Maybe<openGql.grammar.ElementPatternPredicate> predicate) {
    this.variableDeclaration = variableDeclaration;
    this.isLabelExpression = isLabelExpression;
    this.predicate = predicate;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementPatternFiller)) {
      return false;
    }
    ElementPatternFiller o = (ElementPatternFiller) other;
    return java.util.Objects.equals(
      this.variableDeclaration,
      o.variableDeclaration) && java.util.Objects.equals(
      this.isLabelExpression,
      o.isLabelExpression) && java.util.Objects.equals(
      this.predicate,
      o.predicate);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variableDeclaration) + 3 * java.util.Objects.hashCode(isLabelExpression) + 5 * java.util.Objects.hashCode(predicate);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ElementPatternFiller other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variableDeclaration,
      other.variableDeclaration);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      isLabelExpression,
      other.isLabelExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      predicate,
      other.predicate);
  }

  public ElementPatternFiller withVariableDeclaration(hydra.util.Maybe<openGql.grammar.ElementVariableDeclaration> variableDeclaration) {
    return new ElementPatternFiller(variableDeclaration, isLabelExpression, predicate);
  }

  public ElementPatternFiller withIsLabelExpression(hydra.util.Maybe<openGql.grammar.IsLabelExpression> isLabelExpression) {
    return new ElementPatternFiller(variableDeclaration, isLabelExpression, predicate);
  }

  public ElementPatternFiller withPredicate(hydra.util.Maybe<openGql.grammar.ElementPatternPredicate> predicate) {
    return new ElementPatternFiller(variableDeclaration, isLabelExpression, predicate);
  }
}
