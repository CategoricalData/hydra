// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class DateTime implements Serializable, Comparable<DateTime> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.DateTime");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public DateTime (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DateTime)) {
      return false;
    }
    DateTime o = (DateTime) other;
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
  public int compareTo(DateTime other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
