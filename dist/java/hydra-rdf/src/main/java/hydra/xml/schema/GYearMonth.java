// Note: this is an automatically generated file. Do not edit.

package hydra.xml.schema;

import java.io.Serializable;

public class GYearMonth implements Serializable, Comparable<GYearMonth> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.xml.schema.GYearMonth");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public GYearMonth (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GYearMonth)) {
      return false;
    }
    GYearMonth o = (GYearMonth) other;
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
  public int compareTo(GYearMonth other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
