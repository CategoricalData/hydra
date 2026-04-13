// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_ApplyUnary implements Serializable, Comparable<Data_ApplyUnary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_ApplyUnary");

  public static final hydra.core.Name OP = new hydra.core.Name("op");

  public static final hydra.core.Name ARG = new hydra.core.Name("arg");

  public final hydra.scala.syntax.Data_Name op;

  public final hydra.scala.syntax.Data arg;

  public Data_ApplyUnary (hydra.scala.syntax.Data_Name op, hydra.scala.syntax.Data arg) {
    this.op = op;
    this.arg = arg;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ApplyUnary)) {
      return false;
    }
    Data_ApplyUnary o = (Data_ApplyUnary) other;
    return java.util.Objects.equals(
      this.op,
      o.op) && java.util.Objects.equals(
      this.arg,
      o.arg);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(op) + 3 * java.util.Objects.hashCode(arg);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_ApplyUnary other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      op,
      other.op);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      arg,
      other.arg);
  }

  public Data_ApplyUnary withOp(hydra.scala.syntax.Data_Name op) {
    return new Data_ApplyUnary(op, arg);
  }

  public Data_ApplyUnary withArg(hydra.scala.syntax.Data arg) {
    return new Data_ApplyUnary(op, arg);
  }
}
