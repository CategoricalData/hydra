// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class Identifier implements Serializable, Comparable<Identifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.Identifier");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Identifier (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Identifier)) {
      return false;
    }
    Identifier o = (Identifier) other;
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
  public int compareTo(Identifier other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
