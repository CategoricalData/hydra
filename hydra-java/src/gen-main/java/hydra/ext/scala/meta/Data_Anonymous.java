package hydra.ext.scala.meta;

public class Data_Anonymous {
  public Data_Anonymous () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Anonymous)) {
      return false;
    }
    Data_Anonymous o = (Data_Anonymous) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}