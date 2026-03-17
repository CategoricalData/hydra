// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges
 */
public class DataIntersectionOf implements Serializable, Comparable<DataIntersectionOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataIntersectionOf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DataRange> value;

  public DataIntersectionOf (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DataRange> value) {
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
    return ((Comparable) value).compareTo(other.value);
  }
}
