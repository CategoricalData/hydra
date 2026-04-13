// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A component of a qualified name
 */
public class NamePart implements Serializable, Comparable<NamePart> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.NamePart");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public NamePart (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamePart)) {
      return false;
    }
    NamePart o = (NamePart) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NamePart other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
