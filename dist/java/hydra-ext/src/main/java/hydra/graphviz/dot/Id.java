// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class Id implements Serializable, Comparable<Id> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.Id");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Id (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Id)) {
      return false;
    }
    Id o = (Id) other;
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
  public int compareTo(Id other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
