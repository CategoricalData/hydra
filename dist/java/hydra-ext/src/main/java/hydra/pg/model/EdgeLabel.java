// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The label of an edge
 */
public class EdgeLabel implements Serializable, Comparable<EdgeLabel> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.EdgeLabel");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public EdgeLabel (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeLabel)) {
      return false;
    }
    EdgeLabel o = (EdgeLabel) other;
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
  public int compareTo(EdgeLabel other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
