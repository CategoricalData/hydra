// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_New implements Serializable, Comparable<Data_New> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_New");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  public final hydra.scala.syntax.Init init;

  public Data_New (hydra.scala.syntax.Init init) {
    this.init = init;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_New)) {
      return false;
    }
    Data_New o = (Data_New) other;
    return java.util.Objects.equals(
      this.init,
      o.init);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(init);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_New other) {
    return hydra.util.Comparing.compare(
      init,
      other.init);
  }
}
