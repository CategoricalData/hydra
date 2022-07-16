package hydra.ext.java.syntax;

public class ElementValuePair {
  public final Identifier key;
  
  public final ElementValue value;
  
  public ElementValuePair (Identifier key, ElementValue value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementValuePair)) {
      return false;
    }
    ElementValuePair o = (ElementValuePair) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public ElementValuePair withKey(Identifier key) {
    return new ElementValuePair(key, value);
  }
  
  public ElementValuePair withValue(ElementValue value) {
    return new ElementValuePair(key, value);
  }
}