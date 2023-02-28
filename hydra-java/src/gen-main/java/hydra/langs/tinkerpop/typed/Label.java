package hydra.langs.tinkerpop.typed;

/**
 * A vertex or edge label
 */
public class Label {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/typed.Label");
  
  /**
   * A vertex or edge label
   */
  public final String value;
  
  public Label (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Label)) {
      return false;
    }
    Label o = (Label) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}