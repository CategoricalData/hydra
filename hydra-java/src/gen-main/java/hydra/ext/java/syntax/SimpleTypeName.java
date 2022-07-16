package hydra.ext.java.syntax;

public class SimpleTypeName {
  public final TypeIdentifier value;
  
  public SimpleTypeName (TypeIdentifier value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleTypeName)) {
      return false;
    }
    SimpleTypeName o = (SimpleTypeName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}