// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Apply implements Serializable, Comparable<Type_Apply> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Apply");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.ext.scala.meta.Type tpe;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Type> args;

  public Type_Apply (hydra.ext.scala.meta.Type tpe, hydra.util.ConsList<hydra.ext.scala.meta.Type> args) {
    this.tpe = tpe;
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Apply)) {
      return false;
    }
    Type_Apply o = (Type_Apply) other;
    return java.util.Objects.equals(
      this.tpe,
      o.tpe) && java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tpe) + 3 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Apply other) {
    int cmp = 0;
    cmp = ((Comparable) tpe).compareTo(other.tpe);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) args).compareTo(other.args);
  }

  public Type_Apply withTpe(hydra.ext.scala.meta.Type tpe) {
    return new Type_Apply(tpe, args);
  }

  public Type_Apply withArgs(hydra.util.ConsList<hydra.ext.scala.meta.Type> args) {
    return new Type_Apply(tpe, args);
  }
}
