// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_EndMarker implements Serializable, Comparable<Data_EndMarker> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_EndMarker");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.ext.scala.syntax.Data_Name name;

  public Data_EndMarker (hydra.ext.scala.syntax.Data_Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_EndMarker)) {
      return false;
    }
    Data_EndMarker o = (Data_EndMarker) other;
    return java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_EndMarker other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
