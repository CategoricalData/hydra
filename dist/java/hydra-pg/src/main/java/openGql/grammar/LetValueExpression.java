// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class LetValueExpression implements Serializable, Comparable<LetValueExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LetValueExpression");

  public static final hydra.core.Name LET_VARIABLES = new hydra.core.Name("letVariables");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public final java.util.List<openGql.grammar.LetVariableDefinition> letVariables;

  public final openGql.grammar.ValueExpression valueExpression;

  public LetValueExpression (java.util.List<openGql.grammar.LetVariableDefinition> letVariables, openGql.grammar.ValueExpression valueExpression) {
    this.letVariables = letVariables;
    this.valueExpression = valueExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetValueExpression)) {
      return false;
    }
    LetValueExpression o = (LetValueExpression) other;
    return java.util.Objects.equals(
      this.letVariables,
      o.letVariables) && java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(letVariables) + 3 * java.util.Objects.hashCode(valueExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetValueExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      letVariables,
      other.letVariables);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
  }

  public LetValueExpression withLetVariables(java.util.List<openGql.grammar.LetVariableDefinition> letVariables) {
    return new LetValueExpression(letVariables, valueExpression);
  }

  public LetValueExpression withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new LetValueExpression(letVariables, valueExpression);
  }
}
