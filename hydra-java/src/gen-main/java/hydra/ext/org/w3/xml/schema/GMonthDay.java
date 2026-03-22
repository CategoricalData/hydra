// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class GMonthDay implements Serializable, Comparable<GMonthDay> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.GMonthDay");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public GMonthDay (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GMonthDay)) {
      return false;
    }
    GMonthDay o = (GMonthDay) other;
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
  public int compareTo(GMonthDay other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
