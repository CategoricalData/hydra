// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_ApplyInfix implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_ApplyInfix");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OP = new hydra.core.Name("op");
  
  public static final hydra.core.Name FIELD_NAME_TARGS = new hydra.core.Name("targs");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final hydra.ext.scala.meta.Data lhs;
  
  public final hydra.ext.scala.meta.Data_Name op;
  
  public final java.util.List<hydra.ext.scala.meta.Type> targs;
  
  public final java.util.List<hydra.ext.scala.meta.Data> args;
  
  public Data_ApplyInfix (hydra.ext.scala.meta.Data lhs, hydra.ext.scala.meta.Data_Name op, java.util.List<hydra.ext.scala.meta.Type> targs, java.util.List<hydra.ext.scala.meta.Data> args) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((op));
    java.util.Objects.requireNonNull((targs));
    java.util.Objects.requireNonNull((args));
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
    Data_ApplyInfix o = (Data_ApplyInfix) (other);
    return lhs.equals(o.lhs) && op.equals(o.op) && targs.equals(o.targs) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * op.hashCode() + 5 * targs.hashCode() + 7 * args.hashCode();
  }
  
  public Data_ApplyInfix withLhs(hydra.ext.scala.meta.Data lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Data_ApplyInfix(lhs, op, targs, args);
  }
  
  public Data_ApplyInfix withOp(hydra.ext.scala.meta.Data_Name op) {
    java.util.Objects.requireNonNull((op));
    return new Data_ApplyInfix(lhs, op, targs, args);
  }
  
  public Data_ApplyInfix withTargs(java.util.List<hydra.ext.scala.meta.Type> targs) {
    java.util.Objects.requireNonNull((targs));
    return new Data_ApplyInfix(lhs, op, targs, args);
  }
  
  public Data_ApplyInfix withArgs(java.util.List<hydra.ext.scala.meta.Data> args) {
    java.util.Objects.requireNonNull((args));
    return new Data_ApplyInfix(lhs, op, targs, args);
  }
}