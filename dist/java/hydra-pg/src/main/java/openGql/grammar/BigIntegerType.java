// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BigIntegerType implements Serializable, Comparable<BigIntegerType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BigIntegerType");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public BigIntegerType (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BigIntegerType)) {
      return false;
    }
    BigIntegerType o = (BigIntegerType) other;
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
  public int compareTo(BigIntegerType other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
