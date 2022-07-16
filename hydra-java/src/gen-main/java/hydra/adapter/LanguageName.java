package hydra.adapter;

public class LanguageName {
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