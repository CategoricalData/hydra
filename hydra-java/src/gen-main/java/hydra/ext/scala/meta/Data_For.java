// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_For implements Serializable, Comparable<Data_For> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Data_For");

  public static final hydra.core.Name ENUMS = new hydra.core.Name("enums");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Enumerator> enums;

  public Data_For (hydra.util.ConsList<hydra.ext.scala.meta.Enumerator> enums) {
    this.enums = enums;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_For)) {
      return false;
    }
    Data_For o = (Data_For) other;
    return java.util.Objects.equals(
      this.enums,
      o.enums);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(enums);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_For other) {
    return ((Comparable) enums).compareTo(other.enums);
  }
}
