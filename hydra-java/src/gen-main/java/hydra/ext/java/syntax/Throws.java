package hydra.ext.java.syntax;

public class Throws {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Throws");
  
  public final java.util.List<hydra.ext.java.syntax.ExceptionType> value;
  
  public Throws (java.util.List<hydra.ext.java.syntax.ExceptionType> value) {
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