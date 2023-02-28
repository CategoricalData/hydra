package hydra.langs.java.syntax;

/**
 * A Type which does not allow annotations
 */
public class UnannType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.UnannType");
  
  /**
   * A Type which does not allow annotations
   */
  public final hydra.langs.java.syntax.Type value;
  
  public UnannType (hydra.langs.java.syntax.Type value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnannType)) {
      return false;
    }
    UnannType o = (UnannType) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}