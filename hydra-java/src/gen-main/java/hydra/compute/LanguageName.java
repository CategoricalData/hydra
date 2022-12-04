package hydra.compute;

/**
 * The unique name of a language
 */
public class LanguageName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.LanguageName");
  
  /**
   * The unique name of a language
   */
  public final String value;
  
  public LanguageName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LanguageName)) {
      return false;
    }
    LanguageName o = (LanguageName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}