// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Uint64Type implements Serializable, Comparable<Uint64Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Uint64Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Uint64Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Uint64Type)) {
      return false;
    }
    Uint64Type o = (Uint64Type) other;
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
  public int compareTo(Uint64Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
