// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class VarbinaryType implements Serializable, Comparable<VarbinaryType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.VarbinaryType");

  public static final hydra.core.Name MAX_LENGTH = new hydra.core.Name("maxLength");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength;

  public final Boolean notNull;

  public VarbinaryType (hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength, Boolean notNull) {
    this.maxLength = maxLength;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VarbinaryType)) {
      return false;
    }
    VarbinaryType o = (VarbinaryType) other;
    return java.util.Objects.equals(
      this.maxLength,
      o.maxLength) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(maxLength) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VarbinaryType other) {
    int cmp = 0;
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

  public VarbinaryType withMaxLength(hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength) {
    return new VarbinaryType(maxLength, notNull);
  }

  public VarbinaryType withNotNull(Boolean notNull) {
    return new VarbinaryType(maxLength, notNull);
  }
}
