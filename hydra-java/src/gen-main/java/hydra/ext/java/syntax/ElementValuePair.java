package hydra.ext.java.syntax;

public class ElementValuePair {
  public final hydra.ext.java.syntax.Identifier key;
  
  public final hydra.ext.java.syntax.ElementValue value;
  
  public ElementValuePair (hydra.ext.java.syntax.Identifier key, hydra.ext.java.syntax.ElementValue value) {
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
  
  public ElementValuePair withKey(hydra.ext.java.syntax.Identifier key) {
    return new ElementValuePair(key, value);
  }
  
  public ElementValuePair withValue(hydra.ext.java.syntax.ElementValue value) {
    return new ElementValuePair(key, value);
  }
}