// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Float16Type implements Serializable, Comparable<Float16Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Float16Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Float16Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Float16Type)) {
      return false;
    }
    Float16Type o = (Float16Type) other;
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
  public int compareTo(Float16Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
