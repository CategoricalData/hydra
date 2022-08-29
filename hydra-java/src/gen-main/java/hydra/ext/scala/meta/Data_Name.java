package hydra.ext.scala.meta;

public class Data_Name {
  public final hydra.ext.scala.meta.PredefString value;
  
  public Data_Name (hydra.ext.scala.meta.PredefString value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Name)) {
      return false;
    }
    Data_Name o = (Data_Name) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}