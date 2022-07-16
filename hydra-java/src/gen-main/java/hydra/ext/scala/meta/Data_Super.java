package hydra.ext.scala.meta;

public class Data_Super {
  public final Name thisp;
  
  public final Name superp;
  
  public Data_Super (Name thisp, Name superp) {
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
  
  public Data_Super withThisp(Name thisp) {
    return new Data_Super(thisp, superp);
  }
  
  public Data_Super withSuperp(Name superp) {
    return new Data_Super(thisp, superp);
  }
}