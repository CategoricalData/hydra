// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Int128Type implements Serializable, Comparable<Int128Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Int128Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Int128Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Int128Type)) {
      return false;
    }
    Int128Type o = (Int128Type) other;
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
  public int compareTo(Int128Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
