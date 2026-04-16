// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TrimMultiCharacterCharacterString implements Serializable, Comparable<TrimMultiCharacterCharacterString> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TrimMultiCharacterCharacterString");

  public static final hydra.core.Name TRIM_TYPE = new hydra.core.Name("trimType");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name OPTIONAL_VALUE_EXPRESSION = new hydra.core.Name("optionalValueExpression");

  public final openGql.grammar.TrimType trimType;

  public final openGql.grammar.ValueExpression valueExpression;

  public final hydra.util.Maybe<openGql.grammar.ValueExpression> optionalValueExpression;

  public TrimMultiCharacterCharacterString (openGql.grammar.TrimType trimType, openGql.grammar.ValueExpression valueExpression, hydra.util.Maybe<openGql.grammar.ValueExpression> optionalValueExpression) {
    this.trimType = trimType;
    this.valueExpression = valueExpression;
    this.optionalValueExpression = optionalValueExpression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TrimMultiCharacterCharacterString)) {
      return false;
    }
    TrimMultiCharacterCharacterString o = (TrimMultiCharacterCharacterString) other;
    return java.util.Objects.equals(
      this.trimType,
      o.trimType) && java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.optionalValueExpression,
      o.optionalValueExpression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(trimType) + 3 * java.util.Objects.hashCode(valueExpression) + 5 * java.util.Objects.hashCode(optionalValueExpression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TrimMultiCharacterCharacterString other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      trimType,
      other.trimType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      optionalValueExpression,
      other.optionalValueExpression);
  }

  public TrimMultiCharacterCharacterString withTrimType(openGql.grammar.TrimType trimType) {
    return new TrimMultiCharacterCharacterString(trimType, valueExpression, optionalValueExpression);
  }

  public TrimMultiCharacterCharacterString withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new TrimMultiCharacterCharacterString(trimType, valueExpression, optionalValueExpression);
  }

  public TrimMultiCharacterCharacterString withOptionalValueExpression(hydra.util.Maybe<openGql.grammar.ValueExpression> optionalValueExpression) {
    return new TrimMultiCharacterCharacterString(trimType, valueExpression, optionalValueExpression);
  }
}
