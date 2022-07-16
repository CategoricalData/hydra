package hydra.ext.scala.meta;

public class Data_If {
  public final Data cond;
  
  public final Data thenp;
  
  public final Data elsep;
  
  public Data_If (Data cond, Data thenp, Data elsep) {
    this.cond = cond;
    this.thenp = thenp;
    this.elsep = elsep;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_If)) {
      return false;
    }
    Data_If o = (Data_If) (other);
    return cond.equals(o.cond) && thenp.equals(o.thenp) && elsep.equals(o.elsep);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * thenp.hashCode() + 5 * elsep.hashCode();
  }
  
  public Data_If withCond(Data cond) {
    return new Data_If(cond, thenp, elsep);
  }
  
  public Data_If withThenp(Data thenp) {
    return new Data_If(cond, thenp, elsep);
  }
  
  public Data_If withElsep(Data elsep) {
    return new Data_If(cond, thenp, elsep);
  }
}