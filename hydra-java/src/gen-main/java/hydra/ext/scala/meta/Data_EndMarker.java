package hydra.ext.scala.meta;

public class Data_EndMarker {
  public final hydra.ext.scala.meta.Data_Name name;
  
  public Data_EndMarker (hydra.ext.scala.meta.Data_Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_EndMarker)) {
      return false;
    }
    Data_EndMarker o = (Data_EndMarker) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}