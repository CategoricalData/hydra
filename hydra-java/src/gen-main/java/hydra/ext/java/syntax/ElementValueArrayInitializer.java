package hydra.ext.java.syntax;

public class ElementValueArrayInitializer {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ElementValueArrayInitializer");
  
  public final java.util.List<hydra.ext.java.syntax.ElementValue> value;
  
  public ElementValueArrayInitializer (java.util.List<hydra.ext.java.syntax.ElementValue> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementValueArrayInitializer)) {
      return false;
    }
    ElementValueArrayInitializer o = (ElementValueArrayInitializer) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}