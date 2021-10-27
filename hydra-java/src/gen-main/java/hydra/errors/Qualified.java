package hydra.errors;

public class Qualified<A> {
  public final java.util.Optional<A> value;
  
  public final java.util.List<String> warnings;
  
  /**
   * Constructs an immutable Qualified object
   */
  public Qualified(java.util.Optional<A> value, java.util.List<String> warnings) {
    this.value = value;
    this.warnings = warnings;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Qualified)) {
        return false;
    }
    Qualified o = (Qualified) other;
    return value.equals(o.value)
        && warnings.equals(o.warnings);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode()
        + 3 * warnings.hashCode();
  }
  
  /**
   * Construct a new immutable Qualified object in which value is overridden
   */
  public Qualified withValue(java.util.Optional<A> value) {
    return new Qualified(value, warnings);
  }
  
  /**
   * Construct a new immutable Qualified object in which warnings is overridden
   */
  public Qualified withWarnings(java.util.List<String> warnings) {
    return new Qualified(value, warnings);
  }
}
