package hydra.ext.tinkerpop.v3;

/**
 * A map of property keys to property values
 */
public class Properties {
  /**
   * A map of property keys to property values
   */
  public final java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, hydra.core.Literal> value;
  
  public Properties (java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, hydra.core.Literal> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Properties)) {
      return false;
    }
    Properties o = (Properties) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}