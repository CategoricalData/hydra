// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Apply implements Serializable, Comparable<Data_Apply> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Apply");

  public static final hydra.core.Name FUN = new hydra.core.Name("fun");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.ext.scala.syntax.Data fun;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Data> args;

  public Data_Apply (hydra.ext.scala.syntax.Data fun, hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    this.fun = fun;
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Apply)) {
      return false;
    }
    Data_Apply o = (Data_Apply) other;
    return java.util.Objects.equals(
      this.fun,
      o.fun) && java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fun) + 3 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Apply other) {
    int cmp = 0;
    cmp = ((Comparable) fun).compareTo(other.fun);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) args).compareTo(other.args);
  }

  public Data_Apply withFun(hydra.ext.scala.syntax.Data fun) {
    return new Data_Apply(fun, args);
  }

  public Data_Apply withArgs(hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    return new Data_Apply(fun, args);
  }
}
