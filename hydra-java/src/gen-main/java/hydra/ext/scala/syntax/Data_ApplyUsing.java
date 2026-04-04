// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_ApplyUsing implements Serializable, Comparable<Data_ApplyUsing> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_ApplyUsing");

  public static final hydra.core.Name FUN = new hydra.core.Name("fun");

  public static final hydra.core.Name TARGS = new hydra.core.Name("targs");

  public final hydra.ext.scala.syntax.Data fun;

  public final java.util.List<hydra.ext.scala.syntax.Data> targs;

  public Data_ApplyUsing (hydra.ext.scala.syntax.Data fun, java.util.List<hydra.ext.scala.syntax.Data> targs) {
    this.fun = fun;
    this.targs = targs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ApplyUsing)) {
      return false;
    }
    Data_ApplyUsing o = (Data_ApplyUsing) other;
    return java.util.Objects.equals(
      this.fun,
      o.fun) && java.util.Objects.equals(
      this.targs,
      o.targs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fun) + 3 * java.util.Objects.hashCode(targs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_ApplyUsing other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      fun,
      other.fun);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      targs,
      other.targs);
  }

  public Data_ApplyUsing withFun(hydra.ext.scala.syntax.Data fun) {
    return new Data_ApplyUsing(fun, targs);
  }

  public Data_ApplyUsing withTargs(java.util.List<hydra.ext.scala.syntax.Data> targs) {
    return new Data_ApplyUsing(fun, targs);
  }
}
