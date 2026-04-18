// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NormalizeCharacterString implements Serializable, Comparable<NormalizeCharacterString> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NormalizeCharacterString");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name NORMAL_FORM = new hydra.core.Name("normalForm");

  public final openGql.grammar.ValueExpression valueExpression;

  public final hydra.util.Maybe<openGql.grammar.NormalForm> normalForm;

  public NormalizeCharacterString (openGql.grammar.ValueExpression valueExpression, hydra.util.Maybe<openGql.grammar.NormalForm> normalForm) {
    this.valueExpression = valueExpression;
    this.normalForm = normalForm;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalizeCharacterString)) {
      return false;
    }
    NormalizeCharacterString o = (NormalizeCharacterString) other;
    return java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.normalForm,
      o.normalForm);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueExpression) + 3 * java.util.Objects.hashCode(normalForm);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NormalizeCharacterString other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      normalForm,
      other.normalForm);
  }

  public NormalizeCharacterString withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new NormalizeCharacterString(valueExpression, normalForm);
  }

  public NormalizeCharacterString withNormalForm(hydra.util.Maybe<openGql.grammar.NormalForm> normalForm) {
    return new NormalizeCharacterString(valueExpression, normalForm);
  }
}
