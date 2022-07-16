package hydra.ext.scala.meta;

public class Self {
  public Self () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Self)) {
      return false;
    }
    Self o = (Self) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}