package hydra.ext.scala.meta;

public class Data_Placeholder {
  public Data_Placeholder () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Placeholder)) {
      return false;
    }
    Data_Placeholder o = (Data_Placeholder) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}