package hydra.ext.java.syntax;

public class Dims {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Dims");
  
  public final java.util.List<java.util.List<hydra.ext.java.syntax.Annotation>> value;
  
  public Dims (java.util.List<java.util.List<hydra.ext.java.syntax.Annotation>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Dims)) {
      return false;
    }
    Dims o = (Dims) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}