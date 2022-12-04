package hydra.phantoms;

/**
 * An association with a term-level field with a phantom type
 */
public class Fld<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/phantoms.Fld");
  
  public final hydra.core.Field<hydra.compute.Meta> value;
  
  public Fld (hydra.core.Field<hydra.compute.Meta> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fld)) {
      return false;
    }
    Fld o = (Fld) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}