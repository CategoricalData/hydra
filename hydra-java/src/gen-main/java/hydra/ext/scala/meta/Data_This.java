package hydra.ext.scala.meta;

public class Data_This {
  public Data_This () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_This)) {
      return false;
    }
    Data_This o = (Data_This) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}