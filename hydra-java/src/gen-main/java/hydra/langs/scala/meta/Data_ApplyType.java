// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Data_ApplyType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Data.ApplyType");
  
  public final hydra.langs.scala.meta.Data lhs;
  
  public final hydra.langs.scala.meta.Data_Name op;
  
  public final java.util.List<hydra.langs.scala.meta.Type> targs;
  
  public final java.util.List<hydra.langs.scala.meta.Data> args;
  
  public Data_ApplyType (hydra.langs.scala.meta.Data lhs, hydra.langs.scala.meta.Data_Name op, java.util.List<hydra.langs.scala.meta.Type> targs, java.util.List<hydra.langs.scala.meta.Data> args) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    if (op == null) {
      throw new IllegalArgumentException("null value for 'op' argument");
    }
    if (targs == null) {
      throw new IllegalArgumentException("null value for 'targs' argument");
    }
    if (args == null) {
      throw new IllegalArgumentException("null value for 'args' argument");
    }
    this.lhs = lhs;
    this.op = op;
    this.targs = targs;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ApplyType)) {
      return false;
    }
    Data_ApplyType o = (Data_ApplyType) (other);
    return lhs.equals(o.lhs) && op.equals(o.op) && targs.equals(o.targs) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * op.hashCode() + 5 * targs.hashCode() + 7 * args.hashCode();
  }
  
  public Data_ApplyType withLhs(hydra.langs.scala.meta.Data lhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withOp(hydra.langs.scala.meta.Data_Name op) {
    if (op == null) {
      throw new IllegalArgumentException("null value for 'op' argument");
    }
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withTargs(java.util.List<hydra.langs.scala.meta.Type> targs) {
    if (targs == null) {
      throw new IllegalArgumentException("null value for 'targs' argument");
    }
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withArgs(java.util.List<hydra.langs.scala.meta.Data> args) {
    if (args == null) {
      throw new IllegalArgumentException("null value for 'args' argument");
    }
    return new Data_ApplyType(lhs, op, targs, args);
  }
}