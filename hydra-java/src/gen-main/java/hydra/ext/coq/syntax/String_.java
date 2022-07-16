package hydra.ext.coq.syntax;

public class String_ {
  public final String value;
  
  public String_ (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof String_)) {
      return false;
    }
    String_ o = (String_) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}