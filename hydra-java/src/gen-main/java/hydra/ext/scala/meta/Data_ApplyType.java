package hydra.ext.scala.meta;

public class Data_ApplyType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.ApplyType");
  
  public final hydra.ext.scala.meta.Data lhs;
  
  public final hydra.ext.scala.meta.Data_Name op;
  
  public final java.util.List<hydra.ext.scala.meta.Type> targs;
  
  public final java.util.List<hydra.ext.scala.meta.Data> args;
  
  public Data_ApplyType (hydra.ext.scala.meta.Data lhs, hydra.ext.scala.meta.Data_Name op, java.util.List<hydra.ext.scala.meta.Type> targs, java.util.List<hydra.ext.scala.meta.Data> args) {
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
  
  public Data_ApplyType withLhs(hydra.ext.scala.meta.Data lhs) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withOp(hydra.ext.scala.meta.Data_Name op) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withTargs(java.util.List<hydra.ext.scala.meta.Type> targs) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withArgs(java.util.List<hydra.ext.scala.meta.Data> args) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
}