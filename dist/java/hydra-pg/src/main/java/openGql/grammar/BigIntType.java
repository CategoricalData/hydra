// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BigIntType implements Serializable, Comparable<BigIntType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BigIntType");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public BigIntType (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BigIntType)) {
      return false;
    }
    BigIntType o = (BigIntType) other;
    return java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BigIntType other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
