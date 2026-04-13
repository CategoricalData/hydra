// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class Return implements Serializable, Comparable<Return> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.Return");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.cypher.openCypher.ProjectionBody value;

  public Return (hydra.cypher.openCypher.ProjectionBody value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Return)) {
      return false;
    }
    Return o = (Return) other;
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
  public int compareTo(Return other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
