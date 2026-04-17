// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class InsertElementPatternFiller implements Serializable, Comparable<InsertElementPatternFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.InsertElementPatternFiller");

  public static final hydra.core.Name VARIABLE_DECLARATION = new hydra.core.Name("variableDeclaration");

  public static final hydra.core.Name LABEL_AND_PROPERTIES = new hydra.core.Name("labelAndProperties");

  public final hydra.util.Maybe<openGql.grammar.ElementVariableDeclaration> variableDeclaration;

  public final hydra.util.Maybe<openGql.grammar.LabelAndPropertySetSpecification> labelAndProperties;

  public InsertElementPatternFiller (hydra.util.Maybe<openGql.grammar.ElementVariableDeclaration> variableDeclaration, hydra.util.Maybe<openGql.grammar.LabelAndPropertySetSpecification> labelAndProperties) {
    this.variableDeclaration = variableDeclaration;
    this.labelAndProperties = labelAndProperties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InsertElementPatternFiller)) {
      return false;
    }
    InsertElementPatternFiller o = (InsertElementPatternFiller) other;
    return java.util.Objects.equals(
      this.variableDeclaration,
      o.variableDeclaration) && java.util.Objects.equals(
      this.labelAndProperties,
      o.labelAndProperties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variableDeclaration) + 3 * java.util.Objects.hashCode(labelAndProperties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InsertElementPatternFiller other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variableDeclaration,
      other.variableDeclaration);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      labelAndProperties,
      other.labelAndProperties);
  }

  public InsertElementPatternFiller withVariableDeclaration(hydra.util.Maybe<openGql.grammar.ElementVariableDeclaration> variableDeclaration) {
    return new InsertElementPatternFiller(variableDeclaration, labelAndProperties);
  }

  public InsertElementPatternFiller withLabelAndProperties(hydra.util.Maybe<openGql.grammar.LabelAndPropertySetSpecification> labelAndProperties) {
    return new InsertElementPatternFiller(variableDeclaration, labelAndProperties);
  }
}
