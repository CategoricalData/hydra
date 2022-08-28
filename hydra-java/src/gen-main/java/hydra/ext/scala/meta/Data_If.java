package hydra.ext.scala.meta;

public class Data_If {
  public final hydra.ext.scala.meta.Data cond;
  
  public final hydra.ext.scala.meta.Data thenp;
  
  public final hydra.ext.scala.meta.Data elsep;
  
  public Data_If (hydra.ext.scala.meta.Data cond, hydra.ext.scala.meta.Data thenp, hydra.ext.scala.meta.Data elsep) {
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
  
  public Data_If withCond(hydra.ext.scala.meta.Data cond) {
    return new Data_If(cond, thenp, elsep);
  }
  
  public Data_If withThenp(hydra.ext.scala.meta.Data thenp) {
    return new Data_If(cond, thenp, elsep);
  }
  
  public Data_If withElsep(hydra.ext.scala.meta.Data elsep) {
    return new Data_If(cond, thenp, elsep);
  }
}