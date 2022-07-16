package hydra.ext.scala.meta;

public class Data_New {
  public final Init init;
  
  public Data_New (Init init) {
    this.init = init;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_New)) {
      return false;
    }
    Data_New o = (Data_New) (other);
    return init.equals(o.init);
  }
  
  @Override
  public int hashCode() {
    return 2 * init.hashCode();
  }
}