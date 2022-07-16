package hydra.ext.scala.meta;

public class Data_Assign {
  public final Data lhs;
  
  public final Data rhs;
  
  public Data_Assign (Data lhs, Data rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Assign)) {
      return false;
    }
    Data_Assign o = (Data_Assign) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Data_Assign withLhs(Data lhs) {
    return new Data_Assign(lhs, rhs);
  }
  
  public Data_Assign withRhs(Data rhs) {
    return new Data_Assign(lhs, rhs);
  }
}