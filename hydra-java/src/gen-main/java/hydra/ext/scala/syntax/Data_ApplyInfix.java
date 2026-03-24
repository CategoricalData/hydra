// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_ApplyInfix implements Serializable, Comparable<Data_ApplyInfix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_ApplyInfix");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name OP = new hydra.core.Name("op");

  public static final hydra.core.Name TARGS = new hydra.core.Name("targs");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.ext.scala.syntax.Data lhs;

  public final hydra.ext.scala.syntax.Data_Name op;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Type> targs;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Data> args;

  public Data_ApplyInfix (hydra.ext.scala.syntax.Data lhs, hydra.ext.scala.syntax.Data_Name op, hydra.util.ConsList<hydra.ext.scala.syntax.Type> targs, hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    this.lhs = lhs;
    this.op = op;
    this.targs = targs;
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ApplyInfix)) {
      return false;
    }
    Data_ApplyInfix o = (Data_ApplyInfix) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.op,
      o.op) && java.util.Objects.equals(
      this.targs,
      o.targs) && java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(op) + 5 * java.util.Objects.hashCode(targs) + 7 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_ApplyInfix other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) op).compareTo(other.op);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) targs).compareTo(other.targs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) args).compareTo(other.args);
  }

  public Data_ApplyInfix withLhs(hydra.ext.scala.syntax.Data lhs) {
    return new Data_ApplyInfix(lhs, op, targs, args);
  }

  public Data_ApplyInfix withOp(hydra.ext.scala.syntax.Data_Name op) {
    return new Data_ApplyInfix(lhs, op, targs, args);
  }

  public Data_ApplyInfix withTargs(hydra.util.ConsList<hydra.ext.scala.syntax.Type> targs) {
    return new Data_ApplyInfix(lhs, op, targs, args);
  }

  public Data_ApplyInfix withArgs(hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    return new Data_ApplyInfix(lhs, op, targs, args);
  }
}
