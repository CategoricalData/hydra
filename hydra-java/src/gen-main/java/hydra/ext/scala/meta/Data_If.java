// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_If implements Serializable, Comparable<Data_If> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Data_If");

  public static final hydra.core.Name COND = new hydra.core.Name("cond");

  public static final hydra.core.Name THENP = new hydra.core.Name("thenp");

  public static final hydra.core.Name ELSEP = new hydra.core.Name("elsep");

  public final hydra.ext.scala.meta.Data cond;

  public final hydra.ext.scala.meta.Data thenp;

  public final hydra.ext.scala.meta.Data elsep;

  public Data_If (hydra.ext.scala.meta.Data cond, hydra.ext.scala.meta.Data thenp, hydra.ext.scala.meta.Data elsep) {
    this.cond = cond;
    this.thenp = thenp;
    this.elsep = elsep;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_If)) {
      return false;
    }
    Data_If o = (Data_If) other;
    return java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.thenp,
      o.thenp) && java.util.Objects.equals(
      this.elsep,
      o.elsep);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cond) + 3 * java.util.Objects.hashCode(thenp) + 5 * java.util.Objects.hashCode(elsep);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_If other) {
    int cmp = 0;
    cmp = ((Comparable) cond).compareTo(other.cond);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) thenp).compareTo(other.thenp);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) elsep).compareTo(other.elsep);
  }

  public Data_If withCond(hydra.ext.scala.meta.Data cond) {
    return new Data_If(cond, thenp, elsep);
  }

  public Data_If withThenp(hydra.ext.scala.meta.Data thenp) {
    return new Data_If(cond, thenp, elsep);
  }

  public Data_If withElsep(hydra.ext.scala.meta.Data elsep) {
    return new Data_If(cond, thenp, elsep);
  }
}
