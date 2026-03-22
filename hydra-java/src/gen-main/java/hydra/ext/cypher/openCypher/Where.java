// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Where implements Serializable, Comparable<Where> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Where");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.cypher.openCypher.Expression value;

  public Where (hydra.ext.cypher.openCypher.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Where)) {
      return false;
    }
    Where o = (Where) other;
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
  public int compareTo(Where other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
