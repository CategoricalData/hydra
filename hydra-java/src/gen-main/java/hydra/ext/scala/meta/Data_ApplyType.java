package hydra.ext.scala.meta;

public class Data_ApplyType {
  public final Data lhs;
  
  public final Data_Name op;
  
  public final java.util.List<Type> targs;
  
  public final java.util.List<Data> args;
  
  public Data_ApplyType (Data lhs, Data_Name op, java.util.List<Type> targs, java.util.List<Data> args) {
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
  
  public Data_ApplyType withLhs(Data lhs) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withOp(Data_Name op) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withTargs(java.util.List<Type> targs) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
  
  public Data_ApplyType withArgs(java.util.List<Data> args) {
    return new Data_ApplyType(lhs, op, targs, args);
  }
}