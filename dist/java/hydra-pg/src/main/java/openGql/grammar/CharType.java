// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CharType implements Serializable, Comparable<CharType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CharType");

  public static final hydra.core.Name FIXED_LENGTH = new hydra.core.Name("fixedLength");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> fixedLength;

  public final Boolean notNull;

  public CharType (hydra.util.Maybe<openGql.grammar.UnsignedInteger> fixedLength, Boolean notNull) {
    this.fixedLength = fixedLength;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CharType)) {
      return false;
    }
    CharType o = (CharType) other;
    return java.util.Objects.equals(
      this.fixedLength,
      o.fixedLength) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fixedLength) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CharType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      fixedLength,
      other.fixedLength);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public CharType withFixedLength(hydra.util.Maybe<openGql.grammar.UnsignedInteger> fixedLength) {
    return new CharType(fixedLength, notNull);
  }

  public CharType withNotNull(Boolean notNull) {
    return new CharType(fixedLength, notNull);
  }
}
