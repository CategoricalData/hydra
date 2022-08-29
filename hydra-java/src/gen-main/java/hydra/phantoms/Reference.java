package hydra.phantoms;

public class Reference<A> {
  public Reference () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Reference)) {
      return false;
    }
    Reference o = (Reference) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}