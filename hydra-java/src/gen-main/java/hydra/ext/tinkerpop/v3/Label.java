package hydra.ext.tinkerpop.v3;

/**
 * A vertex or edge label
 */
public class Label {
  public final String value;
  
  /**
   * Constructs an immutable Label object
   */
  public Label(String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Label)) {
        return false;
    }
    Label o = (Label) other;
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
