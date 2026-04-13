// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_For implements Serializable, Comparable<Data_For> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_For");

  public static final hydra.core.Name ENUMS = new hydra.core.Name("enums");

  public final java.util.List<hydra.scala.syntax.Enumerator> enums;

  public Data_For (java.util.List<hydra.scala.syntax.Enumerator> enums) {
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
    return hydra.util.Comparing.compare(
      enums,
      other.enums);
  }
}
