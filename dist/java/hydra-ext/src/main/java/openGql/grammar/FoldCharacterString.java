// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FoldCharacterString implements Serializable, Comparable<FoldCharacterString> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FoldCharacterString");

  public static final hydra.core.Name CASE = new hydra.core.Name("case");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public final openGql.grammar.Case case_;

  public final openGql.grammar.ValueExpression valueExpression;

  public FoldCharacterString (openGql.grammar.Case case_, openGql.grammar.ValueExpression valueExpression) {
    this.case_ = case_;
    this.valueExpression = valueExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FoldCharacterString)) {
      return false;
    }
    FoldCharacterString o = (FoldCharacterString) other;
    return java.util.Objects.equals(
      this.case_,
      o.case_) && java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(case_) + 3 * java.util.Objects.hashCode(valueExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FoldCharacterString other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      case_,
      other.case_);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
  }

  public FoldCharacterString withCase(openGql.grammar.Case case_) {
    return new FoldCharacterString(case_, valueExpression);
  }

  public FoldCharacterString withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new FoldCharacterString(case_, valueExpression);
  }
}
