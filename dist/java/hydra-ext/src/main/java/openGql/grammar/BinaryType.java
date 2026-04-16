// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BinaryType implements Serializable, Comparable<BinaryType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BinaryType");

  public static final hydra.core.Name FIXED_LENGTH = new hydra.core.Name("fixedLength");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> fixedLength;

  public final Boolean notNull;

  public BinaryType (hydra.util.Maybe<openGql.grammar.UnsignedInteger> fixedLength, Boolean notNull) {
    this.fixedLength = fixedLength;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BinaryType)) {
      return false;
    }
    BinaryType o = (BinaryType) other;
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
  public int compareTo(BinaryType other) {
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

  public BinaryType withFixedLength(hydra.util.Maybe<openGql.grammar.UnsignedInteger> fixedLength) {
    return new BinaryType(fixedLength, notNull);
  }

  public BinaryType withNotNull(Boolean notNull) {
    return new BinaryType(fixedLength, notNull);
  }
}
