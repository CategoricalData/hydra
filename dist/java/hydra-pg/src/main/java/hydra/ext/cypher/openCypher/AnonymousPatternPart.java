// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class AnonymousPatternPart implements Serializable, Comparable<AnonymousPatternPart> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.AnonymousPatternPart");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.cypher.openCypher.PatternElement value;

  public AnonymousPatternPart (hydra.ext.cypher.openCypher.PatternElement value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnonymousPatternPart)) {
      return false;
    }
    AnonymousPatternPart o = (AnonymousPatternPart) other;
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
  public int compareTo(AnonymousPatternPart other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
