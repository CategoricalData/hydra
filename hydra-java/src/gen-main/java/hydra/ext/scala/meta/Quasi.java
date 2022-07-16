package hydra.ext.scala.meta;

public class Quasi {
  public Quasi () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Quasi)) {
      return false;
    }
    Quasi o = (Quasi) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}