package hydra.ext.scala.meta;

public class Data_Super {
  public final hydra.ext.scala.meta.Name thisp;
  
  public final hydra.ext.scala.meta.Name superp;
  
  public Data_Super (hydra.ext.scala.meta.Name thisp, hydra.ext.scala.meta.Name superp) {
    this.thisp = thisp;
    this.superp = superp;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Super)) {
      return false;
    }
    Data_Super o = (Data_Super) (other);
    return thisp.equals(o.thisp) && superp.equals(o.superp);
  }
  
  @Override
  public int hashCode() {
    return 2 * thisp.hashCode() + 3 * superp.hashCode();
  }
  
  public Data_Super withThisp(hydra.ext.scala.meta.Name thisp) {
    return new Data_Super(thisp, superp);
  }
  
  public Data_Super withSuperp(hydra.ext.scala.meta.Name superp) {
    return new Data_Super(thisp, superp);
  }
}