package hydra.errors;

/**
 * An optional value qualified with a (possibly empty) list of warnings
 */
public class Qualified<M> {
  public final java.util.Optional<M> value;
  
  public final java.util.List<String> warnings;
  
  public Qualified (java.util.Optional<M> value, java.util.List<String> warnings) {
    this.value = value;
    this.warnings = warnings;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Qualified)) {
      return false;
    }
    Qualified o = (Qualified) (other);
    return value.equals(o.value) && warnings.equals(o.warnings);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * warnings.hashCode();
  }
  
  public Qualified withValue(java.util.Optional<M> value) {
    return new Qualified(value, warnings);
  }
  
  public Qualified withWarnings(java.util.List<String> warnings) {
    return new Qualified(value, warnings);
  }
}