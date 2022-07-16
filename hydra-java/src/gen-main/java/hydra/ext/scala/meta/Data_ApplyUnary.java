package hydra.ext.scala.meta;

public class Data_ApplyUnary {
  public final Data_Name op;
  
  public final Data arg;
  
  public Data_ApplyUnary (Data_Name op, Data arg) {
    this.op = op;
    this.arg = arg;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ApplyUnary)) {
      return false;
    }
    Data_ApplyUnary o = (Data_ApplyUnary) (other);
    return op.equals(o.op) && arg.equals(o.arg);
  }
  
  @Override
  public int hashCode() {
    return 2 * op.hashCode() + 3 * arg.hashCode();
  }
  
  public Data_ApplyUnary withOp(Data_Name op) {
    return new Data_ApplyUnary(op, arg);
  }
  
  public Data_ApplyUnary withArg(Data arg) {
    return new Data_ApplyUnary(op, arg);
  }
}