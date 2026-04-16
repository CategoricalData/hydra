// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class USmallIntType implements Serializable, Comparable<USmallIntType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.USmallIntType");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public USmallIntType (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof USmallIntType)) {
      return false;
    }
    USmallIntType o = (USmallIntType) other;
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
  public int compareTo(USmallIntType other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
