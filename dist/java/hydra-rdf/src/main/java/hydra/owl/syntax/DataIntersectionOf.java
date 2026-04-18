// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges
 */
public class DataIntersectionOf implements Serializable, Comparable<DataIntersectionOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.DataIntersectionOf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.owl.syntax.DataRange> value;

  public DataIntersectionOf (java.util.List<hydra.owl.syntax.DataRange> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataIntersectionOf)) {
      return false;
    }
    DataIntersectionOf o = (DataIntersectionOf) other;
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
  public int compareTo(DataIntersectionOf other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
