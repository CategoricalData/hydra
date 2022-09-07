package hydra.ext.java.syntax;

public class MarkerAnnotation {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.MarkerAnnotation");
  
  public final hydra.ext.java.syntax.TypeName value;
  
  public MarkerAnnotation (hydra.ext.java.syntax.TypeName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MarkerAnnotation)) {
      return false;
    }
    MarkerAnnotation o = (MarkerAnnotation) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}