// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Int64Type implements Serializable, Comparable<Int64Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Int64Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Int64Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Int64Type)) {
      return false;
    }
    Int64Type o = (Int64Type) other;
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
  public int compareTo(Int64Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
