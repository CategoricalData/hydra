// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Super implements Serializable, Comparable<Data_Super> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Super");

  public static final hydra.core.Name THISP = new hydra.core.Name("thisp");

  public static final hydra.core.Name SUPERP = new hydra.core.Name("superp");

  public final hydra.ext.scala.syntax.Name thisp;

  public final hydra.ext.scala.syntax.Name superp;

  public Data_Super (hydra.ext.scala.syntax.Name thisp, hydra.ext.scala.syntax.Name superp) {
    this.thisp = thisp;
    this.superp = superp;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Super)) {
      return false;
    }
    Data_Super o = (Data_Super) other;
    return java.util.Objects.equals(
      this.thisp,
      o.thisp) && java.util.Objects.equals(
      this.superp,
      o.superp);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(thisp) + 3 * java.util.Objects.hashCode(superp);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Super other) {
    int cmp = 0;
    cmp = ((Comparable) thisp).compareTo(other.thisp);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) superp).compareTo(other.superp);
  }

  public Data_Super withThisp(hydra.ext.scala.syntax.Name thisp) {
    return new Data_Super(thisp, superp);
  }

  public Data_Super withSuperp(hydra.ext.scala.syntax.Name superp) {
    return new Data_Super(thisp, superp);
  }
}
