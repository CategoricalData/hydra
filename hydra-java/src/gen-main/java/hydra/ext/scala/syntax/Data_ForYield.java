// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_ForYield implements Serializable, Comparable<Data_ForYield> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_ForYield");

  public static final hydra.core.Name ENUMS = new hydra.core.Name("enums");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Enumerator> enums;

  public Data_ForYield (hydra.util.ConsList<hydra.ext.scala.syntax.Enumerator> enums) {
    this.enums = enums;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ForYield)) {
      return false;
    }
    Data_ForYield o = (Data_ForYield) other;
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
  public int compareTo(Data_ForYield other) {
    return ((Comparable) enums).compareTo(other.enums);
  }
}
