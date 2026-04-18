// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Uint16Type implements Serializable, Comparable<Uint16Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Uint16Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Uint16Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Uint16Type)) {
      return false;
    }
    Uint16Type o = (Uint16Type) other;
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
  public int compareTo(Uint16Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
