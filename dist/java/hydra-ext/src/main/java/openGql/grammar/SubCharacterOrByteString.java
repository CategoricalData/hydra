// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SubCharacterOrByteString implements Serializable, Comparable<SubCharacterOrByteString> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SubCharacterOrByteString");

  public static final hydra.core.Name SIDE = new hydra.core.Name("side");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name STRING_LENGTH = new hydra.core.Name("stringLength");

  public final openGql.grammar.Side side;

  public final openGql.grammar.ValueExpression valueExpression;

  public final openGql.grammar.NumericValueExpression stringLength;

  public SubCharacterOrByteString (openGql.grammar.Side side, openGql.grammar.ValueExpression valueExpression, openGql.grammar.NumericValueExpression stringLength) {
    this.side = side;
    this.valueExpression = valueExpression;
    this.stringLength = stringLength;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubCharacterOrByteString)) {
      return false;
    }
    SubCharacterOrByteString o = (SubCharacterOrByteString) other;
    return java.util.Objects.equals(
      this.side,
      o.side) && java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.stringLength,
      o.stringLength);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(side) + 3 * java.util.Objects.hashCode(valueExpression) + 5 * java.util.Objects.hashCode(stringLength);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SubCharacterOrByteString other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      side,
      other.side);
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
      stringLength,
      other.stringLength);
  }

  public SubCharacterOrByteString withSide(openGql.grammar.Side side) {
    return new SubCharacterOrByteString(side, valueExpression, stringLength);
  }

  public SubCharacterOrByteString withValueExpression(openGql.grammar.ValueExpression valueExpression) {
    return new SubCharacterOrByteString(side, valueExpression, stringLength);
  }

  public SubCharacterOrByteString withStringLength(openGql.grammar.NumericValueExpression stringLength) {
    return new SubCharacterOrByteString(side, valueExpression, stringLength);
  }
}
