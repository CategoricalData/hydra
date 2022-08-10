package hydra.ext.java.syntax;

public class Throws {
  public final java.util.List<ExceptionType> value;
  
  public Throws (java.util.List<ExceptionType> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Throws)) {
      return false;
    }
    Throws o = (Throws) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}