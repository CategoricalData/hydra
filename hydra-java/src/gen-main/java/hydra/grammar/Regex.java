package hydra.grammar;

public class Regex {
  public final String value;
  
  public Regex (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Regex)) {
      return false;
    }
    Regex o = (Regex) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}