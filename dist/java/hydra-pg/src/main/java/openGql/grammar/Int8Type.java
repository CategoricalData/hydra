// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Int8Type implements Serializable, Comparable<Int8Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Int8Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Int8Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Int8Type)) {
      return false;
    }
    Int8Type o = (Int8Type) other;
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
  public int compareTo(Int8Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
