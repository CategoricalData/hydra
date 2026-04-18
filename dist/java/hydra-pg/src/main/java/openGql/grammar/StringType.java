// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class StringType implements Serializable, Comparable<StringType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.StringType");

  public static final hydra.core.Name MIN_LENGTH = new hydra.core.Name("minLength");

  public static final hydra.core.Name MAX_LENGTH = new hydra.core.Name("maxLength");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> minLength;

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength;

  public final Boolean notNull;

  public StringType (hydra.util.Maybe<openGql.grammar.UnsignedInteger> minLength, hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength, Boolean notNull) {
    this.minLength = minLength;
    this.maxLength = maxLength;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringType)) {
      return false;
    }
    StringType o = (StringType) other;
    return java.util.Objects.equals(
      this.minLength,
      o.minLength) && java.util.Objects.equals(
      this.maxLength,
      o.maxLength) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(minLength) + 3 * java.util.Objects.hashCode(maxLength) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      minLength,
      other.minLength);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      maxLength,
      other.maxLength);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public StringType withMinLength(hydra.util.Maybe<openGql.grammar.UnsignedInteger> minLength) {
    return new StringType(minLength, maxLength, notNull);
  }

  public StringType withMaxLength(hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength) {
    return new StringType(minLength, maxLength, notNull);
  }

  public StringType withNotNull(Boolean notNull) {
    return new StringType(minLength, maxLength, notNull);
  }
}
