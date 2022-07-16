package hydra.ext.java.syntax;

public class ElementValueArrayInitializer {
  public final java.util.List<ElementValue> value;
  
  public ElementValueArrayInitializer (java.util.List<ElementValue> value) {
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