package hydra.ext.scala.meta;

public class Enumerator_Guard {
  public final Data cond;
  
  public Enumerator_Guard (Data cond) {
    this.cond = cond;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Guard)) {
      return false;
    }
    Enumerator_Guard o = (Enumerator_Guard) (other);
    return cond.equals(o.cond);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode();
  }
}