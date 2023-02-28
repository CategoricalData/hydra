package hydra.langs.tinkerpop.typed;

/**
 * A property key or map key
 */
public class Key {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/typed.Key");
  
  /**
   * A property key or map key
   */
  public final String value;
  
  public Key (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Key)) {
      return false;
    }
    Key o = (Key) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}