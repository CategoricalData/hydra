// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Float128Type implements Serializable, Comparable<Float128Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Float128Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Float128Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Float128Type)) {
      return false;
    }
    Float128Type o = (Float128Type) other;
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
  public int compareTo(Float128Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
