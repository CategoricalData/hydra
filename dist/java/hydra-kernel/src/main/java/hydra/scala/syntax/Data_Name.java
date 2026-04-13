// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_Name implements Serializable, Comparable<Data_Name> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_Name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.scala.syntax.PredefString value;

  public Data_Name (hydra.scala.syntax.PredefString value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Name)) {
      return false;
    }
    Data_Name o = (Data_Name) other;
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
  public int compareTo(Data_Name other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
