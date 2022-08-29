package hydra.ext.java.syntax;

public class DefaultValue {
  public final hydra.ext.java.syntax.ElementValue value;
  
  public DefaultValue (hydra.ext.java.syntax.ElementValue value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DefaultValue)) {
      return false;
    }
    DefaultValue o = (DefaultValue) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}