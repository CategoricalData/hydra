// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Anonymous implements Serializable, Comparable<Data_Anonymous> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Anonymous");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.lang.Void value;

  public Data_Anonymous (java.lang.Void value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Anonymous)) {
      return false;
    }
    Data_Anonymous o = (Data_Anonymous) other;
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
  public int compareTo(Data_Anonymous other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
