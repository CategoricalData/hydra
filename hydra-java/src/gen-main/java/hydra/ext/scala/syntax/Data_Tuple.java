// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Tuple implements Serializable, Comparable<Data_Tuple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Tuple");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Data> args;

  public Data_Tuple (hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Tuple)) {
      return false;
    }
    Data_Tuple o = (Data_Tuple) other;
    return java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Tuple other) {
    return ((Comparable) args).compareTo(other.args);
  }
}
