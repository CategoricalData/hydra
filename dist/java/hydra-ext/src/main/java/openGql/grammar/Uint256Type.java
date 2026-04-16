// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Uint256Type implements Serializable, Comparable<Uint256Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Uint256Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Uint256Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Uint256Type)) {
      return false;
    }
    Uint256Type o = (Uint256Type) other;
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
  public int compareTo(Uint256Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
