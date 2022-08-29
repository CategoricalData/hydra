package hydra.phantoms;

public class Datum<A> {
  public final hydra.core.Term<hydra.core.Meta> value;
  
  public Datum (hydra.core.Term<hydra.core.Meta> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Datum)) {
      return false;
    }
    Datum o = (Datum) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}